GENERIC MODULE PointEvaluator(Field, Type);
IMPORT LRVector;
IMPORT Thread;
IMPORT Debug;

VAR doDebug := Debug.DebugThis("PointEvaluator");

REVEAL
  T = Public BRANDED Brand OBJECT
    c       : Thread.Condition;
    
    (* semaphore *)
    done : BOOLEAN;
    
    (* input vars *)
    p    : LRVector.T;
    func : Field.T;

    (* quit var *)
    doQuit : BOOLEAN;

    (* output var *)
    res    : Field.Result;
  OVERRIDES
    init  := Init;
    apply := Apply;
    start := Start;
    wait  := Wait;
    quit  := Quit;
  END;

VAR
  mu := NEW(MUTEX);
  running := 0;

PROCEDURE Apply(cl : T) : REFANY =
  BEGIN
    LOOP
      LOCK mu DO
        WHILE cl.done DO
          Thread.Wait(mu, cl.c)
        END
      END;

      IF doDebug THEN Debug.Out("Robust.m3 : LinMinApply : done FALSE.") END;

      (* NOT cl.done *)
      IF cl.doQuit THEN
        EXIT
      END;
        
      LOCK mu DO INC(running) END;

      VAR
        result : Field.Result := cl.func.eval(cl.p);
      BEGIN
        IF doDebug THEN Debug.Out("Result " & Type.Format(result)) END;
        LOCK mu DO
          cl.res  := result;
          cl.done := TRUE;
          DEC(running);
          Thread.Signal(cl.c)
        END
      END
    END;
    <*ASSERT FALSE*>
  END Apply;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.c := NEW(Thread.Condition);
    t.done := TRUE;
    t.doQuit := FALSE;
    EVAL Thread.Fork(t);
    RETURN t
  END Init;

PROCEDURE Start(t    : T;
                p    : LRVector.T;
                func : Field.T) =
  BEGIN
    LOCK mu DO
      <*ASSERT t.done = TRUE*>
      t.p := p;
      t.func := func;
      t.done := FALSE;
      Thread.Signal(t.c) 
    END
  END Start;

PROCEDURE Wait(t : T) : Field.Result =
  BEGIN
    LOCK mu DO
      WHILE NOT t.done DO
        Thread.Wait(mu, t.c);
      END;
      RETURN t.res
    END
  END Wait;

PROCEDURE Quit(t : T) =
  BEGIN
    LOCK mu DO
      t.doQuit := TRUE;
      Thread.Signal(t.c)
    END
  END Quit;

PROCEDURE Running() : CARDINAL =
  BEGIN LOCK mu DO RETURN running END END Running;

BEGIN END PointEvaluator.
