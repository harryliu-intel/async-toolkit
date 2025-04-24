MODULE CspCompiledScheduler;
IMPORT CspCompiledProcess AS Process;
IMPORT Word;
FROM Fmt IMPORT Int;
IMPORT Debug;

CONST doDebug = TRUE;
      
PROCEDURE Schedule(closure : Process.Closure) =
  BEGIN
    <*ASSERT closure # NIL*>

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


VAR
  active, next := NEW(REF ARRAY OF Process.Closure, 1);
  ap, np := 0;
  nexttime : Word.T := 0;

PROCEDURE Run() =
  BEGIN
    (* run *)

    LOOP
      IF doDebug THEN Debug.Out("Scheduling loop : np = " & Int(np)) END;

      IF np = 0 THEN
        Debug.Error("DEADLOCK!")
      END;
      
      VAR
        temp := active;
      BEGIN
        active := next;
        ap := np;
        next := temp;
        np := 0;
      END;

      INC(nexttime);

      FOR i := 0 TO ap - 1 DO
        WITH cl      = active[i] DO
          <*ASSERT cl # NIL*>
          cl.run();
          (*IF NOT success THEN Schedule(cl) END*)
        END
      END
    END
  END Run;
  
(**********************************************************************)

BEGIN END CspCompiledScheduler.
