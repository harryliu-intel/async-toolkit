MODULE CspMaster;
IMPORT Thread;
FROM Fmt IMPORT F, Int;
IMPORT Debug;
IMPORT ProcUtils;
IMPORT Wr;
IMPORT Rd;
IMPORT TextReader;
IMPORT IP;
IMPORT Scan;
IMPORT Lex, FloatMode;
IMPORT Text;
FROM CspSimUtils IMPORT ScanIp, FmtIp;
FROM CspSim IMPORT Builder;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED Brand OBJECT
    nworkers : CARDINAL;
    cmd      : TEXT;
    wcl      : REF ARRAY OF Manager;
    bld      : Builder;
    mt       : CARDINAL; (* threads per worker *)
  OVERRIDES
    init := Init;
    run  := Run;
  END;

PROCEDURE NextPow2(q : CARDINAL) : CARDINAL =
  VAR
    z := 1;
  BEGIN
    WHILE z < q DO
      z := z * 2
    END;
    RETURN z
  END NextPow2;
  
PROCEDURE Init(t        : T;
               nworkers : CARDINAL;
               cmd      : TEXT;
               bld      : Builder;
               mt       : CARDINAL) : T =
  BEGIN
    t.nworkers := nworkers;
    t.cmd      := cmd;
    t.bld      := bld;
    t.mt       := NextPow2(mt);
    RETURN t
  END Init;

TYPE
  Manager = Thread.Closure OBJECT
    t     : T;
    id    : CARDINAL;
    thr   : Thread.T;
    ep    : IP.Endpoint;
    state : MgrState;
  OVERRIDES
    apply := MgrApply;
  END;

VAR mu := NEW(MUTEX);
VAR c  := NEW(Thread.Condition);

TYPE MgrState =
{ Starting, Ready, InitWorkers, ConnectWorkers };

CONST MgrStateNames = ARRAY MgrState OF TEXT 
{ "Starting", "Ready", "InitWorkers", "ConnectWorkers" };

PROCEDURE Run(t : T) =

  PROCEDURE AwaitReady() =
    BEGIN
      LOCK mu DO
        FOR i := 0 TO t.nworkers - 1 DO
          WHILE t.wcl[i].state # MgrState.Ready DO
            Thread.Wait(mu, c)
          END
        END
      END
    END AwaitReady;

  PROCEDURE SetState(state : MgrState) =
    BEGIN
      LOCK mu DO
        FOR i := 0 TO t.nworkers - 1 DO
          <*ASSERT t.wcl[i].state = MgrState.Ready*>
          t.wcl[i].state := state
        END
      END;
      Thread.Broadcast(c)
    END SetState;

  BEGIN
    t.wcl := NEW(REF ARRAY OF Manager, t.nworkers);
    Debug.Out(F("CspMaster cmd = \"%s\"", t.cmd));
    FOR i := 0 TO t.nworkers - 1 DO
      WITH mgr = NEW(Manager, t := t, id := i, state := MgrState.Starting) DO
        t.wcl[i] := mgr;
        t.wcl[i].thr := Thread.Fork(mgr)
      END
    END;

    AwaitReady();

    SetState(MgrState.InitWorkers);

    AwaitReady();

    SetState(MgrState.ConnectWorkers);

    AwaitReady();

    Debug.Out("Worker processes ready.");
    Debug.Out("====================  BUILD PROCESS GRAPH  ====================");
    
    t.bld();

    WITH nschedulers = t.mt * t.nworkers DO
      
    END;
    
    LOOP
      Thread.Pause(0.1d0)
    END
  END Run;

CONST Delims = " ";

PROCEDURE MgrApply(mgr : Manager) : REFANY =

  PROCEDURE DoInitWorkers() =
    BEGIN
      Debug.Out(F("MgrApply(%s).DoInitWorkers", Int(mgr.id)));

      Wr.PutText(wr, F("NTHREADS %s\n", Int(mgr.t.mt)));
      Wr.PutText(wr, F("NPEERS %s\n", Int(NUMBER(mgr.t.wcl^))));
      Wr.Flush(wr);

      WITH line   = Rd.GetLine(rd),
           reader = NEW(TextReader.T).init(line),
           kw     = reader.nextE(Delims, TRUE) DO
        <*ASSERT TE(kw, "READY")*>
        Debug.Out(F("MgrApply %s : ready reported.", Int(mgr.id)))
      END
    END DoInitWorkers;

  PROCEDURE DoConnectWorkers() =
    BEGIN
      FOR i := FIRST(mgr.t.wcl^) TO LAST(mgr.t.wcl^) DO
        WITH peerMgr = mgr.t.wcl[i] DO
          Wr.PutText(wr, F("PEER %s %s %s\n",
                           Int(i),
                           FmtIp(peerMgr.ep.addr),
                           Int(peerMgr.ep.port)))
        END
      END;

      Wr.PutText(wr, F("CONNECT\n"));
      
      Wr.Flush(wr)
    END DoConnectWorkers;

  PROCEDURE SetMyselfReady() =
    BEGIN
      LOCK mu DO
        mgr.state := MgrState.Ready
      END;
      Thread.Signal(c)
    END SetMyselfReady;
    
  VAR
    stdin      : ProcUtils.Reader;
    stdout     : ProcUtils.Writer;
    wr         : Wr.T;
    rd         : Rd.T;
    completion : ProcUtils.Completion;
    
    cmd := F(mgr.t.cmd, Int(mgr.id));
  BEGIN
    stdin  := ProcUtils.GimmeWr(wr);
    stdout := ProcUtils.GimmeRd(rd);

    completion := ProcUtils.RunText(cmd,
                                    stdin  := stdin,
                                    stderr := ProcUtils.Stderr(),
                                    stdout := stdout);

    LOOP
      IF Rd.CharsReady(rd) # 0 THEN
        WITH line = Rd.GetLine(rd) DO
          Debug.Out(F("Thread %s got line \"%s\"", Int(mgr.id), line));
          WITH reader = NEW(TextReader.T).init(line),
               kw     = reader.nextE(Delims, TRUE) DO
            IF TE(kw, "WORKER") THEN
              WITH idT = reader.nextE(Delims, TRUE),
                   ipT = reader.nextE(Delims, TRUE),
                   ptT = reader.nextE(Delims, TRUE),

                   id  = Scan.Int(idT),
                   ip  = ScanIp  (ipT),
                   pt  = Scan.Int(ptT),

                   ep  = IP.Endpoint { addr := ip, port := pt } DO
                LOCK mu DO
                  mgr.ep    := ep;
                  mgr.state := MgrState.Ready
                END;
                Thread.Signal(c)
              END
            ELSE
              Debug.Warning("Don't understand worker response : " & line)
            END
          END
        END
      END;
      VAR
        state : MgrState;
      BEGIN
        LOCK mu DO state := mgr.state END;
        CASE state OF
          MgrState.Starting, MgrState.Ready => (* skip *)
        |
          MgrState.InitWorkers =>
          DoInitWorkers();
          SetMyselfReady()
        |
          MgrState.ConnectWorkers =>
          DoConnectWorkers();
          SetMyselfReady()
        ELSE
          Debug.Error(F("manager %s : unexpected state %s",
                        Int(mgr.id), MgrStateNames[state]))
        END
      END;
          
      Thread.Pause(0.3d0)
    END
  END MgrApply;
  
BEGIN END CspMaster.
