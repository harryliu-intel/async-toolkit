MODULE CspMaster;
IMPORT Thread;
FROM Fmt IMPORT F, Int, FN;
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
IMPORT TextCardTbl;
IMPORT CspSim;
IMPORT TextPortTbl;
IMPORT CspPortObject;
IMPORT CspChannel;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

TYPE TA = ARRAY OF TEXT;

REVEAL
  T = Public BRANDED Brand OBJECT
    cmd      : TEXT;
    wcl      : REF ARRAY OF Manager;
    bld      : Builder;
    procMap  : TextCardTbl.T;
    portMap  : TextPortTbl.T;
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
{ Starting, Ready, InitWorkers, ConnectWorkers, ParcelOut };

CONST MgrStateNames = ARRAY MgrState OF TEXT 
{ "Starting", "Ready", "InitWorkers", "ConnectWorkers", "ParcelOut" };

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
    Debug.Out("====================  BUILD LOCAL PROCESS GRAPH  ====================");
    
    t.bld(restrict := NIL);

    WITH nschedulers = t.mt * t.nworkers DO
      t.procMap := AssignSchedulers(t);
    END;
    t.portMap := CspSim.GetPortTbl();
    
    SetState(MgrState.ParcelOut);
    
    AwaitReady();

    LOOP
      Thread.Pause(0.1d0)
    END
  END Run;
  
PROCEDURE AssignSchedulers(t : T) : TextCardTbl.T =
  VAR
    seq     := CspSim.GetProcSeq();
    procMap := NEW(TextCardTbl.Default).init();
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH fr  = seq.get(i),
           wid = i MOD t.nworkers,
           sid = (i DIV t.nworkers) MOD t.mt,
           gid = wid * t.mt + sid DO
        Debug.Out(F("AssignSchedulers :  %s to scheduler %s",
                    fr.name,
                    Int(gid)));
        EVAL procMap.put(fr.name, gid)
      END
    END;
    RETURN procMap
  END AssignSchedulers;

CONST Delims = " ";

PROCEDURE MgrApply(mgr : Manager) : REFANY =

  PROCEDURE Send(str : TEXT) =
    BEGIN
      Debug.Out(F("MgrApply %s, send : %s", Int(mgr.id), str));

      Thread.Pause(0.1d0); (* slow it down a bit for debug *)
      
      Wr.PutText(wr, str);
      Wr.PutChar(wr, '\n')
    END Send;

  PROCEDURE Flush() =
    BEGIN
      Wr.Flush(wr)
    END Flush;
    
  PROCEDURE DoInitWorkers() =
    BEGIN
      Debug.Out(F("MgrApply(%s).DoInitWorkers", Int(mgr.id)));

      Send(F("NTHREADS %s", Int(mgr.t.mt)));
      Send(F("NPEERS %s", Int(NUMBER(mgr.t.wcl^))));
      Flush();

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
          Send(F("PEER %s %s %s",
                           Int(i),
                           FmtIp(peerMgr.ep.addr),
                           Int(peerMgr.ep.port)))
        END
      END;

      Send(F("CONNECT"));
      
      Flush()
    END DoConnectWorkers;

  PROCEDURE DoParcelOut() =
    VAR
      iter := mgr.t.procMap.iterate();
      k : TEXT;
      v : CARDINAL;
    BEGIN
      Send(F("BEGINPROCS"));
      WHILE iter.next(k, v) DO
        IF mgr.t.gid2wid(v) = mgr.id THEN
          Send(F("SCHEDULE %s %s", Int(mgr.t.gid2sid(v)), k))
        END
      END;
      Send(F("ENDPROCS"));
      Flush()
    END DoParcelOut;
    
  PROCEDURE DoTouchedEdges() =
    VAR
      iter := mgr.t.portMap.iterate();
      k : TEXT;
      v : CspPortObject.T;
    BEGIN
      (* here we list all the channels that touch any of the processes 
         assigned to our worker *)
      Send(F("BEGINEDGES"));
      WHILE iter.next(k, v) DO
        TYPECASE v OF
          CspChannel.T(chan) =>
          VAR
            wrs, rds : CARDINAL; (* scheduler IDs of ends of channel *)
          BEGIN
            WITH hadIt = mgr.t.procMap.get(chan.writer.name, wrs) DO
              <*ASSERT hadIt*>
            END;
            WITH hadIt = mgr.t.procMap.get(chan.reader.name, rds) DO
              <*ASSERT hadIt*>
            END;
            
            IF mgr.t.gid2wid(rds) = mgr.id OR mgr.t.gid2wid(wrs) = mgr.id THEN
              Send(FN("CHANNEL %s %s %s %s %s %s %s %s",
                                TA{
              Int(chan.id),
              chan.nm,
              chan.writer.name, Int(chan.writer.id), Int(wrs),
              chan.reader.name, Int(chan.reader.id), Int(rds)
              }))
            END
          END

          (* need to repeat the same for a node *)
          
        ELSE
          (* skip *)
        END
      END;
      Send(F("ENDEDGES"));
      Flush()
    END DoTouchedEdges;

  PROCEDURE DoCommandBuild() =
    BEGIN
      Send("BUILD");
      Flush()
    END DoCommandBuild;

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
        |
          MgrState.ParcelOut =>
          DoParcelOut();
          DoTouchedEdges();
          DoCommandBuild();
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
