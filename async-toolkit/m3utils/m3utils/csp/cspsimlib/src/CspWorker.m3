MODULE CspWorker;
IMPORT TCP;
IMPORT IP;
IMPORT Thread;
IMPORT Debug;
IMPORT ConnRW;
IMPORT Wr, Rd;
FROM Fmt IMPORT F, Int, FN;
IMPORT Stdio;
FROM CspSimUtils IMPORT ScanIp, FmtIp;
IMPORT TextReader;
IMPORT Scan;
IMPORT FloatMode, Lex;
IMPORT Text;
IMPORT TextSet, TextSetDef;
IMPORT TextCardTbl;
IMPORT CspSim;
IMPORT TextRefTbl;

<*FATAL Thread.Alerted*>

TYPE
  TA = ARRAY OF TEXT;

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED Brand OBJECT
    conn        : TCP.Connector;
    ep          : IP.Endpoint;
    listener    : Thread.T; (* listen for incoming connections *)
    manager     : Thread.T; (* handle incoming commands from master *)
    mu          : MUTEX;
    c           : Thread.Condition;
    id          : CARDINAL;
    peers       : REF ARRAY OF Peer;
    nthreads    : CARDINAL;
    initialized : BOOLEAN;
    myprocs     : TextCardTbl.T;
    bld         : CspSim.Builder;
  OVERRIDES
    init                := Init;
    getEp               := GetEp;
    getThread           := GetThread;
    awaitInitialization := AwaitInitialization;
  END;

TYPE
  Peer = RECORD
    ep     : IP.Endpoint;
    worker : Worker;
  END;
  
PROCEDURE GetThread(t : T) : Thread.T =
  BEGIN
    RETURN t.manager
  END GetThread;

PROCEDURE Init(t : T; id : CARDINAL; bld : CspSim.Builder) : T =
  BEGIN
    Debug.Out(F("CspWorker.Init(id = %s)", Int(id)));
    WITH nullEp = IP.Endpoint { IP.NullAddress, IP.NullPort } DO
      t.conn := TCP.NewConnector(nullEp)
    END;
    t.initialized := FALSE;
    t.ep          := TCP.GetEndPoint(t.conn);
    t.mu          := NEW(MUTEX);
    t.c           := NEW(Thread.Condition);
    t.id          := id;
    t.listener    := Thread.Fork(NEW(Listener, t := t, apply := ListenApply)); 
    t.manager     := Thread.Fork(NEW(Listener, t := t, apply := CommandApply));
    t.myprocs     := NEW(TextCardTbl.Default).init();
    t.bld         := bld;
    
    Wr.PutText(Stdio.stdout,
               FN("WORKER %s %s %s\n",
                  TA{Int(t.id),
                     FmtIp(t.ep.addr),
                     Int(t.ep.port)
    } )   );

    Wr.Flush(Stdio.stdout);
    
    RETURN t
  END Init;

PROCEDURE GetEp(t : T) : IP.Endpoint =
  BEGIN
    RETURN t.ep
  END GetEp;

TYPE
  Listener = Thread.Closure OBJECT
    t : T;
  OVERRIDES
    apply := ListenApply;
  END;

  Worker = Thread.Closure OBJECT
    t  : T;
    me : Thread.T;
    wr : Wr.T;
    rd : Rd.T;
  OVERRIDES
    apply := WorkerApply;
  END;

VAR mu := NEW(MUTEX);

CONST Delims = " ";
      
PROCEDURE CommandApply(listener : Listener) : REFANY =

  PROCEDURE Line() : TextReader.T =
    BEGIN
      WITH line =  Rd.GetLine(Stdio.stdin) DO
        Debug.Out(F("CspWorker.Manager %s got \"%s\"",
                    Int(listener.t.id),
                    line));
        RETURN NEW(TextReader.T).init(line)
      END
    END Line;

  VAR
    myprocnames := NEW(TextSetDef.T).init();
    mychans     := NEW(TextRefTbl.Default).init();
  BEGIN
    LOOP
      WITH t      = listener.t,
           reader = Line(),
           kw     = reader.nextE(Delims, TRUE) DO
          IF    TE(kw, "NPEERS") THEN
            WITH n = reader.getInt() DO
              t.peers := NEW(REF ARRAY OF Peer, n)
            END;
            Wr.PutText(Stdio.stdout, "READY\n");
            Wr.Flush(Stdio.stdout)
          ELSIF TE(kw, "NTHREADS") THEN
            WITH n = reader.getInt() DO
              t.nthreads := n
            END;
          ELSIF TE(kw, "PEER") THEN
            WITH idT = reader.nextE(Delims, TRUE),
                 ipT = reader.nextE(Delims, TRUE),
                 ptT = reader.nextE(Delims, TRUE),
                 
                 id  = Scan.Int(idT),
                 ip  = ScanIp  (ipT),
                 pt  = Scan.Int(ptT),
                 
                 ep  = IP.Endpoint { addr := ip, port := pt } DO
              LOCK mu DO
                t.peers[id].ep := ep
              END
            END(*HTIW*)
          ELSIF TE(kw, "BEGINPROCS") THEN
            LOOP
              WITH reader = Line(),
                   kw     = reader.nextE(Delims, TRUE) DO
                IF    TE(kw, "SCHEDULE") THEN
                  WITH sid = Scan.Int(reader.nextE(Delims, TRUE)),
                       pnm = reader.nextE("", TRUE) DO
                    EVAL listener.t.myprocs.put(pnm, sid);
                    EVAL myprocnames.insert(pnm)
                  END
                ELSIF TE(kw, "ENDPROCS") THEN
                  EXIT
                ELSE
                  <*ASSERT FALSE*>
                END
              END
            END
          ELSIF TE(kw, "BEGINEDGES") THEN
            LOOP
              WITH reader = Line(),
                   kw     = reader.nextE(Delims, TRUE) DO
                IF    TE(kw, "CHANNEL") THEN
                  WITH cnm  = reader.nextE(Delims, TRUE),
                       cid  = Scan.Int(reader.nextE(Delims, TRUE)),
                       wrnm = reader.nextE(Delims, TRUE),
                       wrs  = Scan.Int(reader.nextE(Delims, TRUE)),
                       rdnm = reader.nextE(Delims, TRUE),
                       rds  = Scan.Int(reader.nextE(Delims, TRUE)),

                       rc   = NEW(RemoteChannel,
                                  nm   := cnm,
                                  id   := cid,
                                  wrnm := wrnm, wrs := wrs,
                                  rdnm := rdnm, rds := rds) DO
                    EVAL mychans.put(cnm, rc)
                  END
                ELSIF TE(kw, "ENDEDGES") THEN
                  EXIT
                ELSE
                  <*ASSERT FALSE*>
                END
              END
            END
          ELSIF TE(kw, "BUILD") THEN
            Debug.Out(
                F("====================  BUILD WORKER %s PROCESS GRAPH  ====================", Int(t.id)));
            
            t.bld(myprocnames)
            
          ELSIF TE(kw, "CONNECT") THEN
            FOR i := t.id + 1 TO LAST(t.peers^) DO
              WITH  tcp  = TCP.Connect(t.peers[i].ep),
                    w    = NEW(Worker,
                               wr := ConnRW.NewWr(tcp),
                               rd := ConnRW.NewRd(tcp)),
                    wthr = Thread.Fork(w) DO
                w.me := wthr;
                t.peers[i].worker := w;
                Wr.PutText(w.wr, F("HELLOIAM %s\n", Int(t.id)));
                Wr.Flush(w.wr)
              END                
            END;(* ROF *)
            LOCK t.mu DO
              t.initialized := TRUE;
              Thread.Signal(t.c)
            END
          END
        END
    END
  END CommandApply;

TYPE
  RemoteChannel = OBJECT
    nm   : TEXT;
    id   : CARDINAL;
    wrnm : TEXT;
    wrs  : CARDINAL;
    rdnm : TEXT;
    rds  : CARDINAL;
  END;

PROCEDURE ListenApply(listener : Listener) : REFANY =
  BEGIN
    LOOP
      WITH t      = listener.t,
           tcp    = TCP.Accept(t.conn),
           wr     = ConnRW.NewWr(tcp),
           rd     = ConnRW.NewRd(tcp),
           worker = NEW(Worker, t := t, wr := wr, rd := rd),
           wthr   = Thread.Fork(worker)
       DO
        worker.me := wthr;
        Debug.Out("listener accepted incoming")
      END
    END
  END ListenApply;

PROCEDURE WorkerApply(worker : Worker) : REFANY =
  BEGIN
    <*ASSERT worker # NIL*>
    <*ASSERT worker.rd # NIL*>
    LOOP
      WITH line = Rd.GetLine(worker.rd) DO
        Debug.Out(F("CspWorker.Worker %s got \"%s\"", Int(worker.t.id), line));
        WITH reader = NEW(TextReader.T).init(line),
             kw     = reader.nextE(Delims, TRUE) DO
          IF    TE(kw, "HELLOIAM") THEN
            WITH idT = reader.nextE(Delims, TRUE),
                 id  = Scan.Int(idT) DO
              worker.t.peers[id].worker := worker
            END
          END
        END
      END
    END(*POOL*)
  END WorkerApply;

PROCEDURE AwaitInitialization(t : T) =
  BEGIN
    LOCK t.mu DO
      WHILE NOT t.initialized DO
        Thread.Wait(t.mu, t.c)
      END
    END
  END AwaitInitialization;
  
BEGIN END CspWorker.
