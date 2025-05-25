GENERIC MODULE CspCompiledScheduler(CspDebug);
IMPORT CspCompiledProcess AS Process;
IMPORT Word;
FROM Fmt IMPORT Int, F;
IMPORT Debug;
IMPORT CspScheduler;

CONST doDebug = CspDebug.DebugSchedule;
      
PROCEDURE Schedule(closure : Process.Closure) =
  BEGIN
    <*ASSERT closure # NIL*>

    IF doDebug THEN
      IF running = NIL THEN
        Debug.Out(F("NIL : NIL scheduling %s : %s to run",
                  Int(closure.frameId), closure.name))
      ELSE
        Debug.Out(F("%s : %s scheduling %s : %s to run",
                    Int(running.frameId), running.name,
                    Int(closure.frameId), closure.name))
      END
    END;
    
    IF closure.scheduled = nexttime THEN
      IF doDebug THEN
        Debug.Out(Int(closure.id) & " already scheduled at " & Int(nexttime))
      END;
      RETURN 
    END;
    
    IF np > LAST(next^) THEN
      WITH new = NEW(REF ARRAY OF Process.Closure, NUMBER(next^) * 2) DO
        SUBARRAY(new^, 0, NUMBER(next^)) := next^;
        next := new
      END
    END;
    next[np] := closure;
    closure.scheduled := nexttime;
    INC(np)
  END Schedule;

PROCEDURE ScheduleFork(READONLY closures : ARRAY OF Process.Closure) : CARDINAL =
  BEGIN
    FOR i := FIRST(closures) TO LAST(closures) DO
      Schedule(closures[i])
    END;
    RETURN NUMBER(closures)
  END ScheduleFork;

VAR
  active, next := NEW(REF ARRAY OF Process.Closure, 1);
  ap, np := 0;
  nexttime : Word.T := 0;

PROCEDURE GetTime() : Word.T =
  BEGIN RETURN nexttime END GetTime;

VAR
  running : Process.Closure := NIL;
  
PROCEDURE Run() =
  BEGIN
    (* run *)

    LOOP
      IF doDebug THEN Debug.Out("=====  Scheduling loop : np = " & Int(np)) END;

      IF np = 0 THEN
        Debug.Error("DEADLOCK!")
      END;
      
      VAR
        temp := active;
      BEGIN
        active := next;
        ap     := np;
        next   := temp;
        np     := 0;
      END;

      INC(nexttime);

      FOR i := 0 TO ap - 1 DO
        WITH cl      = active[i] DO
          <*ASSERT cl # NIL*>
          IF doDebug THEN
            Debug.Out(F("Scheduler switch to %s : %s",
                        Int(cl.frameId), cl.name));
            running := cl
          END;
          cl.run();
          IF doDebug THEN
            Debug.Out(F("Scheduler switch from %s : %s",
                        Int(cl.frameId), cl.name));
            running := NIL
          END;
          (*IF NOT success THEN Schedule(cl) END*)
        END
      END
    END
  END Run;
  
(**********************************************************************)

BEGIN
  CspScheduler.GetTime := GetTime;
END CspCompiledScheduler.
