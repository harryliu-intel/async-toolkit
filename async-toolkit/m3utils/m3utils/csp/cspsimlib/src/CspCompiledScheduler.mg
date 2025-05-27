GENERIC MODULE CspCompiledScheduler(CspDebug);
IMPORT CspCompiledProcess AS Process;
IMPORT Word;
FROM Fmt IMPORT Int, F;
IMPORT Debug;
IMPORT CspScheduler;
IMPORT CspPortObject;
IMPORT TextFrameTbl;
IMPORT TextPortTbl;
IMPORT CspClosureSeq AS ClosureSeq;
IMPORT CspChannel;

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
    
    np           : CARDINAL;
    running      : Process.Closure;

    outbox       : REF ARRAY OF ClosureSeq.T;
    (* written by "from" scheduler, read by each "to" scheduler, according
       to their ids *)

    rdirty       : REF ARRAY OF CspChannel.T;
    nrp          : CARDINAL;
    wdirty       : REF ARRAY OF CspChannel.T;
    nwp          : CARDINAL;
  END;

PROCEDURE ReadDirty(chan : CspChannel.T; cl : Process.Closure) =
  VAR
    t : T := cl.fr.affinity;
  BEGIN
    IF t.nrp > LAST(t.rdirty^) THEN
      WITH new = NEW(REF ARRAY OF CspChannel.T, NUMBER(t.rdirty^) * 2) DO
        SUBARRAY(new^, 0, NUMBER(t.rdirty^)) := t.rdirty^;
        t.rdirty := new
      END;
    END;
    t.rdirty[t.nrp] := chan;
    INC(t.nrp)
  END ReadDirty;
  
PROCEDURE WriteDirty(chan : CspChannel.T; cl : Process.Closure) =
  VAR
    t : T := cl.fr.affinity;
  BEGIN
    IF t.nwp > LAST(t.wdirty^) THEN
      WITH new = NEW(REF ARRAY OF CspChannel.T, NUMBER(t.wdirty^) * 2) DO
        SUBARRAY(new^, 0, NUMBER(t.wdirty^)) := t.wdirty^;
        t.wdirty := new
      END;
    END;
    t.wdirty[t.nwp] := chan;
    INC(t.nwp)
  END WriteDirty;

PROCEDURE ScheduleOther(from, toSchedule : Process.Closure) =
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
        fromScheduler.outbox[toScheduler.id].addhi(toSchedule)
      END
    END
  END ScheduleOther;
  
PROCEDURE Schedule(closure : Process.Closure) =
  VAR
    t : T := closure.fr.affinity;
  BEGIN
    <*ASSERT closure # NIL*>

    IF doDebug THEN
      Debug.Out(F("scheduling %s : %s [%s] to run",
                  Int(closure.frameId), closure.name, closure.fr.typeName));
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

    IF closure.scheduled = nexttime THEN
      IF doDebug THEN
        Debug.Out(Int(closure.id) & " already scheduled at " & Int(nexttime))
      END;
      RETURN 
    END;
    
    IF t.np > LAST(t.next^) THEN
      WITH new = NEW(REF ARRAY OF Process.Closure, NUMBER(t.next^) * 2) DO
        SUBARRAY(new^, 0, NUMBER(t.next^)) := t.next^;
        t.next := new
      END
    END;
    t.next[t.np] := closure;
    closure.scheduled := nexttime;
    INC(t.np)
  END Schedule;

PROCEDURE ScheduleFork(READONLY closures : ARRAY OF Process.Closure) : CARDINAL =
  BEGIN
    FOR i := FIRST(closures) TO LAST(closures) DO
      Schedule(closures[i])
    END;
    RETURN NUMBER(closures)
  END ScheduleFork;

PROCEDURE GetTime() : Word.T =
  BEGIN RETURN nexttime END GetTime;

VAR nexttime : Word.T := 0;

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
  VAR
    ap : CARDINAL;
  BEGIN
    (* run *)

    LOOP
      IF doDebug THEN
        Debug.Out(F("=====  Scheduling loop %s: np = %s", Int(t.id), Int(t.np))) 
      END;

      IF t.np = 0 THEN
        RETURN
      END;

      VAR
        temp := t.active;
      BEGIN
        t.active := t.next;
        ap       := t.np;
        t.next   := temp;
        t.np     := 0;
      END;

      INC(nexttime);

      FOR i := 0 TO ap - 1 DO
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
    
PROCEDURE Run() =
  BEGIN
    IF schedulers = NIL THEN
      theScheduler := NEW(T,
                          id     := 0,
                          active := NEW(REF ARRAY OF Process.Closure, 1),
                          next   := NEW(REF ARRAY OF Process.Closure, 1),
                          np     := 0);

      (* mark the affinity of each process *)
      VAR
        k : TEXT;
        v : Process.Frame;
        iter := theProcs.iterate();
      BEGIN
        WHILE iter.next(k, v) DO
          v.affinity := theScheduler
        END
      END;
      
      (* start each process *)
      VAR
        k : TEXT;
        v : Process.Frame;
        iter := theProcs.iterate();
      BEGIN
        WHILE iter.next(k, v) DO
          v.start()
        END
      END;
      
      Run1(theScheduler)
    ELSE
      RunMulti(schedulers^)
    END
  END Run;
  
(**********************************************************************)

VAR schedulers : REF ARRAY OF T := NIL;
    (* when running parallel schedulers *)

PROCEDURE RunMulti(READONLY schedulers : ARRAY OF T) =
  BEGIN
  END RunMulti;

PROCEDURE CreateMulti(n : CARDINAL) =
  (* create n schedulers *)
  BEGIN
    schedulers := NEW(REF ARRAY OF T, n);
    FOR i := FIRST(schedulers^) TO LAST(schedulers^) DO
      WITH new = NEW(T,
                     id     := i,
                     active := NEW(REF ARRAY OF Process.Closure, 1),
                     next   := NEW(REF ARRAY OF Process.Closure, 1),
                     np     := 0,

                     rdirty := NEW(REF ARRAY OF CspChannel.T, 1),
                     nrp    := 0,
        
                     wdirty := NEW(REF ARRAY OF CspChannel.T, 1),
                     nwp    := 0
                     ) DO
        schedulers[i] := new;
        new.outbox := NEW(REF ARRAY OF ClosureSeq.T, n);

        FOR j := FIRST(new.outbox^) TO LAST(new.outbox^) DO
          IF j = i THEN
            new.outbox[j] := NIL
          ELSE
            new.outbox[j] := NEW(ClosureSeq.T).init()
          END
        END;
      END(*WITH*)      
    END(*FOR*)
  END CreateMulti;

BEGIN
  CspScheduler.GetTime := GetTime;
END CspCompiledScheduler.
