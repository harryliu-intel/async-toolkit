GENERIC MODULE CspCompiledScheduler(CspDebug);
IMPORT CspCompiledProcess AS Process;
IMPORT Word;
FROM Fmt IMPORT Int, F;
IMPORT Debug;
IMPORT CspScheduler;
IMPORT CspPortObject;
IMPORT TextFrameTbl;
IMPORT TextPortTbl;

CONST doDebug = CspDebug.DebugSchedule;

TYPE
  T = CspScheduler.T OBJECT
    id           : CARDINAL;
    active, next : REF ARRAY OF Process.Closure;
    ap, np       : CARDINAL;
    running      : Process.Closure;
  END;

PROCEDURE ScheduleOther(from, toSchedule : Process.Closure) =
  BEGIN
    IF from.fr.affinity = toSchedule.fr.affinity THEN
      (* the source and target are in the same scheduler, we can 
         schedule them same as if they were local *)
      Schedule(toSchedule)
    ELSE
      (* if the target block is running under another scheduler,
         we do not schedule it directly.  Instead, we put it in the 
         appropriate outbox for our scheduler to handle at the end of the
         timestep *)
      
      <*ASSERT FALSE*> (* not yet *)
      
    END
  END ScheduleOther;
  
PROCEDURE Schedule(closure : Process.Closure) =
  VAR
    t : T := closure.fr.affinity;
  BEGIN
    <*ASSERT closure # NIL*>

    IF doDebug THEN
      IF t.running = NIL THEN
        Debug.Out(F("NIL : NIL scheduling %s : %s to run",
                  Int(closure.frameId), closure.name))
      ELSE
        Debug.Out(F("%s : %s scheduling %s : %s to run",
                    Int(t.running.frameId), t.running.name,
                    Int(closure.frameId), closure.name))
      END
    END;
    
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

VAR
  nexttime : Word.T := 0;


VAR
  theProcs := NEW(TextFrameTbl.Default).init();

PROCEDURE GetProcTbl() : TextFrameTbl.T =
  BEGIN RETURN theProcs END GetProcTbl;

PROCEDURE RegisterProcess(fr : Process.Frame) =
  BEGIN
    fr.affinity := theScheduler;

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
        t.ap     := t.np;
        t.next   := temp;
        t.np     := 0;
      END;

      INC(nexttime);

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

VAR
  theScheduler := NEW(T,
                      id     := 0,
                      active := NEW(REF ARRAY OF Process.Closure, 1),
                      next   := NEW(REF ARRAY OF Process.Closure, 1),
                      ap     := 0,
                      np     := 0);
  
PROCEDURE Run() =
  BEGIN
    Run1(theScheduler);
  END Run;
  
(**********************************************************************)

BEGIN
  CspScheduler.GetTime := GetTime;
END CspCompiledScheduler.
