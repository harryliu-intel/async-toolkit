GENERIC MODULE CspCompiledScheduler(CspDebug);
IMPORT CspCompiledProcess AS Process;
IMPORT Word;
FROM Fmt IMPORT Int, F, Bool;
IMPORT Debug;
IMPORT CspScheduler;
IMPORT CspPortObject;
IMPORT TextFrameTbl;
IMPORT TextPortTbl;
IMPORT CspClosureSeq AS ClosureSeq;
IMPORT CspChannel;
IMPORT Random;
IMPORT Thread;

CONST doDebug = CspDebug.DebugSchedule;

TYPE
  T = CspScheduler.T OBJECT

    (* we should probably consider promoting many of the fields to the
       CspScheduler interface (or adding another private one) so that
       everyone can see them and not have to take the overhead of a 
       NARROW() check so much below *)
    
    id           : CARDINAL;

    active, next : REF ARRAY OF Process.Closure;
    (* this is double-buffered.
       
       The "active" closures are the ones we are running on this iteration;
       the "next" closures are the ones we are scheduling for the next iteration
    *)
    
    ap, np       : CARDINAL;
    running      : Process.Closure;

    (* parallel scheduler fields 
       
       we add these to EVERY scheduler so we don't need to stick a bunch
       of extra tests in the Send/Recv/etc. 
    *)

    
    mu           : MUTEX;
    c            : Thread.Condition;
    
    commOutbox   : REF ARRAY OF ClosureSeq.T;
    waitOutbox   : REF ARRAY OF ClosureSeq.T;
    (* written by "from" scheduler, read by each "to" scheduler, according
       to their ids *)

    (* 
       The following are written by the end that updates.

       They are indexed by the target scheduler, so the target 
       can read the contents without contention.
    *)
    rdirty       : REF ARRAY OF REF ARRAY OF CspChannel.T;
    nrp          : REF ARRAY OF CARDINAL;
    wdirty       : REF ARRAY OF REF ARRAY OF CspChannel.T;
    nwp          : REF ARRAY OF CARDINAL;

    thePhase     : Phase;

    time         : Word.T;
  END;

PROCEDURE ReadDirty(targ : CspChannel.T; cl : Process.Closure) =
  VAR
    t   : T := cl.fr.affinity;
    tgt : T := targ.writer.affinity; (* target (remote) scheduler *)
    tgtId   := tgt.id;               (* target scheduler's id *)
  BEGIN
    IF t.nrp[tgtId] > LAST(t.rdirty[tgtId]^) THEN
      WITH new = NEW(REF ARRAY OF CspChannel.T, NUMBER(t.rdirty[tgtId]^) * 2) DO
        SUBARRAY(new^, 0, NUMBER(t.rdirty[tgtId]^)) := t.rdirty[tgtId]^;
        t.rdirty[tgtId] := new
      END;
    END;
    t.rdirty[tgtId][t.nrp[tgtId]] := targ;
    INC(t.nrp[tgtId])
  END ReadDirty;
  
PROCEDURE WriteDirty(surr : CspChannel.T; cl : Process.Closure) =
  VAR
    t   : T := cl.fr.affinity;
    tgt : T := surr.writer.affinity; (* target (remote) scheduler *)
    tgtId   := tgt.id;               (* target scheduler's id *)
  BEGIN
    IF t.nwp[tgtId] > LAST(t.wdirty[tgtId]^) THEN
      WITH new = NEW(REF ARRAY OF CspChannel.T, NUMBER(t.wdirty[tgtId]^) * 2) DO
        SUBARRAY(new^, 0, NUMBER(t.wdirty[tgtId]^)) := t.wdirty[tgtId]^;
        t.wdirty[tgtId] := new
      END;
    END;
    t.wdirty[tgtId][t.nwp[tgtId]] := surr;
    INC(t.nwp[tgtId])
  END WriteDirty;

PROCEDURE ScheduleComm(from, toSchedule : Process.Closure) =
  VAR
    fromScheduler : T := from.fr.affinity;
  BEGIN
    IF fromScheduler = toSchedule.fr.affinity THEN
      (* the source and target are in the same scheduler, we can 
         schedule them same as if they were local *)
      Schedule(toSchedule)
    ELSE
      (* if the target block is running under another scheduler,
         we do not schedule it directly.  Instead, we put it in the 
         appropriate outbox to handle at the end of the
         timestep *)
      VAR
        toScheduler   : T := toSchedule.fr.affinity;
      BEGIN
        fromScheduler.commOutbox[toScheduler.id].addhi(toSchedule)
      END
    END
  END ScheduleComm;
  
PROCEDURE ScheduleWait(from, toSchedule : Process.Closure) =
  VAR
    fromScheduler : T := from.fr.affinity;
  BEGIN
    IF fromScheduler = toSchedule.fr.affinity THEN
      (* the source and target are in the same scheduler, we can 
         schedule them same as if they were local *)
      IF toSchedule.waiting THEN
        toSchedule.waiting := FALSE;
        Schedule(toSchedule)
      END
    ELSE
      (* if the target block is running under another scheduler,
         we do not schedule it directly.  Instead, we put it in the 
         appropriate outbox to handle at the end of the
         timestep *)
      VAR
        toScheduler   : T := toSchedule.fr.affinity;
      BEGIN
        fromScheduler.waitOutbox[toScheduler.id].addhi(toSchedule)
      END
    END
  END ScheduleWait;
  
PROCEDURE Schedule(closure : Process.Closure) =
  VAR
    t : T := closure.fr.affinity;
  BEGIN
    <*ASSERT closure # NIL*>

    IF doDebug THEN
      Debug.Out(F("scheduling %s : %s [%s] to run at %s",
                  Int(closure.frameId), closure.name, closure.fr.typeName,
                  Int(t.time)));
      IF t.running = NIL THEN
        Debug.Out(F("NIL : NIL scheduling %s : %s [%s] to run",
                  Int(closure.frameId), closure.name, closure.fr.typeName))
      ELSE
        Debug.Out(F("%s : %s scheduling %s : %s [%s] to run",
                    Int(t.running.frameId), t.running.name,
                    Int(closure.frameId), closure.name, closure.fr.typeName))
      END
    END;

    <*ASSERT t # NIL*>

    IF closure.scheduled = t.time THEN
      IF doDebug THEN
        Debug.Out(F("%s : %s already scheduled at %s",
                    Int(closure.frameId), closure.name, Int(t.time)))
      END;
      RETURN 
    END;
    
    IF t.np > LAST(t.next^) THEN
      WITH new = NEW(REF ARRAY OF Process.Closure, NUMBER(t.next^) * 2) DO
        SUBARRAY(new^, 0, NUMBER(t.next^)) := t.next^;
        t.next := new
      END
    END;
    t.next[t.np]      := closure;
    closure.scheduled := t.time;
    INC(t.np);

    IF doDebug THEN
      Debug.Out(F("Schedule : np = %s", Int(t.np)))
    END
  END Schedule;

PROCEDURE ScheduleFork(READONLY closures : ARRAY OF Process.Closure) : CARDINAL =
  BEGIN
    FOR i := FIRST(closures) TO LAST(closures) DO
      Schedule(closures[i])
    END;
    RETURN NUMBER(closures)
  END ScheduleFork;

PROCEDURE GetTime() : Word.T =
  BEGIN RETURN masterTime END GetTime;

VAR masterTime : Word.T := 0;

VAR theProcs := NEW(TextFrameTbl.Default).init();

PROCEDURE GetProcTbl() : TextFrameTbl.T =
  BEGIN RETURN theProcs END GetProcTbl;

PROCEDURE RegisterProcess(fr : Process.Frame) =
  BEGIN
    IF doDebug THEN
      Debug.Out(F("Registering process : %s", fr.name))
    END;
    EVAL theProcs.put(fr.name, fr)
  END RegisterProcess;

VAR
  theEdges := NEW(TextPortTbl.Default).init();
  
PROCEDURE GetPortTbl() : TextPortTbl.T =
  BEGIN RETURN theEdges END GetPortTbl;

PROCEDURE RegisterEdge(edge : CspPortObject.T) =
  BEGIN
    EVAL theEdges.put(edge.nm, edge)
  END RegisterEdge;

PROCEDURE Run1(t : T) =
  BEGIN
    (* run *)
    t.time := masterTime;
    
    LOOP
      IF doDebug THEN
        Debug.Out(F("=====  @ %s Scheduling loop %s: np = %s",
                    Int(t.time),
                    Int(t.id), Int(t.np))) 
      END;

      IF t.np = 0 THEN
        RETURN
      END;

      VAR
        temp := t.active;
      BEGIN
        t.active := t.next;
        t.ap     := t.np;
        t.next   := temp;
        t.np     := 0;
      END;

      (* note that the time switches here, BEFORE we run *)
      INC(masterTime);
      t.time := masterTime;

      FOR i := 0 TO t.ap - 1 DO
        WITH cl      = t.active[i] DO
          <*ASSERT cl # NIL*>
          IF doDebug THEN
            Debug.Out(F("Scheduler switch to %s : %s",
                        Int(cl.frameId), cl.name));
            t.running := cl
          END;
          cl.run();
          IF doDebug THEN
            Debug.Out(F("Scheduler switch from %s : %s",
                        Int(cl.frameId), cl.name));
            t.running := NIL
          END;
          (*IF NOT success THEN Schedule(cl) END*)
        END
      END
    END
  END Run1;

VAR theScheduler : T;
    (* when running a single scheduler *)
    
PROCEDURE Run(mt : CARDINAL; greedy : BOOLEAN) =
  BEGIN
    IF mt = 0 THEN
      theScheduler := NEW(T,
                          id     := 0,
                          active := NEW(REF ARRAY OF Process.Closure, 1),
                          next   := NEW(REF ARRAY OF Process.Closure, 1),
                          np     := 0);

      (* mark the affinity of each process *)
      MapRandomly(ARRAY OF T { theScheduler });
      StartProcesses();
      Run1(theScheduler)
    ELSE
      CreateMulti(mt);
      MapRandomly(schedulers^);
      StartProcesses();
      RunMulti(schedulers^, greedy)
    END
  END Run;

PROCEDURE StartProcesses() =
  (* start each process *)
  VAR
    k : TEXT;
    v : Process.Frame;
    iter := theProcs.iterate();
  BEGIN
    WHILE iter.next(k, v) DO
      v.start()
    END
  END StartProcesses;

PROCEDURE MapRandomly(READONLY sarr : ARRAY OF T) =
  VAR
    rand := NEW(Random.Default).init(TRUE);
    k : TEXT;
    v : Process.Frame;
    iter := theProcs.iterate();
  BEGIN
    WHILE iter.next(k, v) DO
      WITH q = rand.integer(FIRST(sarr), LAST(sarr)) DO
        v.affinity := sarr[q]
      END
    END
  END MapRandomly;
    
(**********************************************************************)

VAR schedulers : REF ARRAY OF T := NIL;
    (* when running parallel schedulers *)

PROCEDURE AcquireLocks(READONLY schedulers : ARRAY OF T) =
  BEGIN
    FOR i := FIRST(schedulers) TO LAST(schedulers) DO
      Thread.Acquire(schedulers[i].mu)
    END
  END AcquireLocks;

PROCEDURE ReleaseLocks(READONLY schedulers : ARRAY OF T) =
  BEGIN
    FOR i := FIRST(schedulers) TO LAST(schedulers) DO
      Thread.Release(schedulers[i].mu)
    END
  END ReleaseLocks;

PROCEDURE SignalThreads(READONLY schedulers : ARRAY OF T) =
  BEGIN
    FOR i := FIRST(schedulers) TO LAST(schedulers) DO
      Thread.Signal(schedulers[i].c)
    END
  END SignalThreads;

PROCEDURE SetPhase(READONLY schedulers : ARRAY OF T; phase : Phase) =
  (* scheduler[].mu m/b locked *)
  BEGIN
    IF doDebug THEN
      Debug.Out(F("SetPhase(%s)", PhaseNames[phase]))
    END;
    FOR i := FIRST(schedulers) TO LAST(schedulers) DO
      schedulers[i].thePhase := phase
    END
  END SetPhase;

PROCEDURE AwaitPhase(READONLY schedulers : ARRAY OF T; phase : Phase) =
  (* scheduler[].mu m/b unlocked *)
  BEGIN
    IF doDebug THEN
      Debug.Out(F("AwaitPhase(%s)", PhaseNames[phase]))
    END;
    FOR i := FIRST(schedulers) TO LAST(schedulers) DO
      WITH s = schedulers[i] DO
        LOCK s.mu DO
          WHILE s.thePhase # phase DO
            Thread.Wait(s.mu, s.c)
          END
        END
      END
    END
  END AwaitPhase;
  
(**********************************************************************)  
    
TYPE
  Phase = {
  Idle,
  GetRemoteBlocks,
  SwapActiveBlocks, (* this could be combined with UpdateSurrogates *)
  UpdateSurrogates,
  RunActiveBlocks
  };

CONST
  PhaseNames = ARRAY Phase OF TEXT {
  "Idle",
  "GetRemoteBlocks",
  "SwapActiveBlocks", 
  "UpdateSurrogates",
  "RunActiveBlocks"
  };

PROCEDURE RunMulti(READONLY schedulers : ARRAY OF T; greedy : BOOLEAN) =

  PROCEDURE NothingPending() : BOOLEAN =
    BEGIN
      (* something is pending if it is in the local queue of a scheduler
         or if it is in the outbox of a scheduler *)
      FOR i := FIRST(schedulers) TO LAST(schedulers) DO
        WITH s = schedulers[i] DO
          IF s.np # 0 THEN RETURN FALSE END;
          FOR j := FIRST(schedulers) TO LAST(schedulers) DO
            IF s.commOutbox[j].size() # 0 THEN RETURN FALSE END;
            IF s.waitOutbox[j].size() # 0 THEN RETURN FALSE END
          END
        END
      END;
      RETURN TRUE
    END NothingPending;
  
  PROCEDURE CountPendingStuff() : CARDINAL =
    VAR
      res : CARDINAL := 0;
    BEGIN
      (* something is pending if it is in the local queue of a scheduler
         or if it is in the outbox of a scheduler *)
      FOR i := FIRST(schedulers) TO LAST(schedulers) DO
        WITH s = schedulers[i] DO
          res := res + s.np;
          
          FOR j := FIRST(schedulers) TO LAST(schedulers) DO
            res := res + s.commOutbox[j].size();
            res := res + s.waitOutbox[j].size()
          END
        END
      END;
      RETURN res
    END CountPendingStuff;
  
  PROCEDURE RunPhase(phase : Phase) =
    BEGIN
      IF doDebug THEN
        Debug.Out(F("RunMulti.RunPhase(%s)", PhaseNames[phase]))
      END;

      FOR i := FIRST(schedulers) TO LAST(schedulers) DO
        <*ASSERT schedulers[i].thePhase = Phase.Idle*>
      END;

      AcquireLocks(schedulers);

      TRY
        SetPhase(schedulers, phase)
      FINALLY
        ReleaseLocks(schedulers)
      END;
      SignalThreads(schedulers);
      AwaitPhase(schedulers, Phase.Idle)
    END RunPhase;

  PROCEDURE UpdateTime() =
    BEGIN
      (* ensure time advances for everybody *)
      FOR i := FIRST(schedulers) TO LAST(schedulers) DO
        masterTime := MAX(masterTime, schedulers[i].time)
      END;
      
      INC(masterTime);
    END UpdateTime;
    
  VAR
    cls   := NEW(REF ARRAY OF SchedClosure, NUMBER(schedulers));
    thrds := NEW(REF ARRAY OF Thread.T    , NUMBER(schedulers));
  BEGIN
    FOR i := FIRST(cls^) TO LAST(cls^) DO
      cls[i]   := NEW(SchedClosure, t := schedulers[i], greedy := greedy);
      thrds[i] := Thread.Fork(cls[i])
    END;

    LOOP
      IF doDebug THEN
        Debug.Out(F("=====  @ %s Master scheduling loop : np = %s",
                    Int(masterTime),
                    Int(CountPendingStuff())
                   )
                 )
      END;

      IF NothingPending() THEN
        IF doDebug THEN
          Debug.Out("RunMulti : nothing pending---done.")
        END;
        RETURN
      END;

      UpdateTime();

      RunPhase(Phase.GetRemoteBlocks);

(*      UpdateTime();  *)
      
      RunPhase(Phase.SwapActiveBlocks);

      RunPhase(Phase.UpdateSurrogates);

      RunPhase(Phase.RunActiveBlocks);
    END
  END RunMulti;

TYPE
  SchedClosure = Thread.Closure OBJECT
    t      : T;
    greedy : BOOLEAN;
  OVERRIDES
    apply := Apply;
  END;

CONST InitPhase = Phase.Idle;
      
PROCEDURE Apply(cl : SchedClosure) : REFANY =

  PROCEDURE DoSwapActiveBlocks() =
    VAR
      temp := t.active;
    BEGIN
      t.active := t.next;
      t.ap     := t.np;
      t.next   := temp;
      t.np     := 0;
    END DoSwapActiveBlocks;
  
  VAR
    t        := cl.t;
    myId     := t.id;
  BEGIN
    IF doDebug THEN
      Debug.Out(F("Starting scheduler %s", Int(myId)))
    END;
    
    LOOP
      (* this is a single scheduler *)

      (* wait for command from central *)
      LOCK t.mu DO
        WHILE t.thePhase = Phase.Idle DO
          Thread.Wait(t.mu, t.c)
        END
      END;

      IF doDebug THEN
        Debug.Out(F("Scheduler(%s) : phase %s",
                    Int(myId),
                    PhaseNames[t.thePhase]))
      END;

      CASE t.thePhase OF
        Phase.Idle =>
        <*ASSERT FALSE*>
      |
        Phase.GetRemoteBlocks =>
        <*ASSERT masterTime > t.time*>
        t.time := masterTime;
        
        FOR r := FIRST(schedulers^) TO LAST(schedulers^) DO
          WITH box = schedulers[r].commOutbox[myId] DO
            FOR i := 0 TO box.size() - 1 DO
              WITH myCl = box.get(i) DO
                Schedule(myCl)
              END
            END
          END;
          WITH box = schedulers[r].waitOutbox[myId] DO
            FOR i := 0 TO box.size() - 1 DO
              WITH myCl = box.get(i) DO
                IF myCl.waiting THEN
                  myCl.waiting := FALSE; (* in case there are multiple wakers *)
                  Schedule(myCl)
                END
              END
            END
          END
        END
      |
        Phase.SwapActiveBlocks =>
        
        (* clear out the schedulers we just copied in GetRemoteBlocks *)
        FOR i := FIRST(schedulers^) TO LAST(schedulers^) DO
          EVAL t.commOutbox[i].init();
          EVAL t.waitOutbox[i].init()
        END;

        DoSwapActiveBlocks()
      |
        Phase.UpdateSurrogates =>
        (* 
           Update the channels with surrogate writes. 

           This needs to be done on the reader side (obv.)
        *)
        FOR i := FIRST(schedulers^) TO LAST(schedulers^) DO
          WITH remote = NARROW(schedulers[i],T) DO
            FOR w := 0 TO remote.nwp[myId] - 1 DO
              WITH surrogate = remote.wdirty[myId][w] DO
                surrogate.writeSurrogate()
              END
            END;
            FOR r := 0 TO remote.nrp[myId] - 1 DO
              WITH target = remote.rdirty[myId][r] DO
                target.readSurrogate()
              END
            END
          END
        END;
      |
        Phase.RunActiveBlocks =>
        (* first zero all the read/write channel stuff *)
        FOR i := FIRST(schedulers^) TO LAST(schedulers^) DO
          t.nrp[i] := 0;
          t.nwp[i] := 0
        END;

        (* now run the user code *)
        LOOP
          FOR i := 0 TO t.ap - 1 DO
            WITH cl      = t.active[i] DO
              <*ASSERT cl # NIL*>
              IF doDebug THEN
                Debug.Out(F("Apply %s : Scheduler switch to %s : %s",
                            Int(myId), Int(cl.frameId), cl.name));
                t.running := cl
              END;
              cl.run();
              IF doDebug THEN
                Debug.Out(F("Apply %s : Scheduler switch from %s : %s",
                            Int(myId), Int(cl.frameId), cl.name));
                t.running := NIL
              END;
              (*IF NOT success THEN Schedule(cl) END*)
            END(*WITH*)
          END(*FOR*);

          IF doDebug THEN
            Debug.Out(F("Apply %s : cl.greedy=%s t.np=%s",
                        Int(myId), Bool(cl.greedy), Int(t.np)))
          END;
          
          IF cl.greedy AND t.np # 0 THEN
            (* just keep running till we are out of things to do *)
            INC(t.time);
            DoSwapActiveBlocks()
          ELSE
            EXIT
          END
        END(*LOOP*)

      END;

      LOCK t.mu DO
        t.thePhase := Phase.Idle
      END;
      (* local time will have updated *)
      Thread.Signal(t.c)
    END
  END Apply;
  
PROCEDURE CreateMulti(n : CARDINAL) =
  (* create n schedulers *)

  PROCEDURE NewChanArray() : REF ARRAY OF REF ARRAY OF CspChannel.T =
    VAR
      res := NEW(REF ARRAY OF REF ARRAY OF CspChannel.T, n);
    BEGIN
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := NEW(REF ARRAY OF CspChannel.T, 1)
      END;
      RETURN res
    END NewChanArray;

  PROCEDURE NewCardArray() : REF ARRAY OF CARDINAL =
    VAR
      res := NEW(REF ARRAY OF CARDINAL, n);
    BEGIN
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := 0
      END;
      RETURN res
    END NewCardArray;
    
  BEGIN
    IF doDebug THEN
      Debug.Out(F("Creating %s schedulers", Int(n)))
    END;
    
    schedulers := NEW(REF ARRAY OF T, n);
    FOR i := FIRST(schedulers^) TO LAST(schedulers^) DO
      WITH new = NEW(T,
                     id       := i,
                     active   := NEW(REF ARRAY OF Process.Closure, 1),
                     next     := NEW(REF ARRAY OF Process.Closure, 1),
                     np       := 0,

                     mu       := NEW(MUTEX),
                     c        := NEW(Thread.Condition),
                     
                     rdirty   := NewChanArray(),
                     nrp      := NewCardArray(),
        
                     wdirty   := NewChanArray(),
                     nwp      := NewCardArray(),

                     thePhase := InitPhase
                     ) DO
        schedulers[i] := new;

        new.commOutbox := NEW(REF ARRAY OF ClosureSeq.T, n);
        new.waitOutbox := NEW(REF ARRAY OF ClosureSeq.T, n);

        FOR j := FIRST(schedulers^) TO LAST(schedulers^) DO
          new.commOutbox[j] := NEW(ClosureSeq.T).init();
          new.waitOutbox[j] := NEW(ClosureSeq.T).init()
        END;
      END(*WITH*)      
    END(*FOR*)
  END CreateMulti;

BEGIN
  CspScheduler.GetTime := GetTime;
END CspCompiledScheduler.
