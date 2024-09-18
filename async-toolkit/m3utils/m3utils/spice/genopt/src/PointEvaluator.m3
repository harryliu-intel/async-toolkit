MODULE PointEvaluator;
IMPORT LRVector;
IMPORT LRScalarField;
IMPORT Thread;
IMPORT Debug;
FROM Fmt IMPORT LongReal;

CONST LR = LongReal;

REVEAL
  T = Public BRANDED Brand OBJECT
    c       : Thread.Condition;
    
    (* semaphore *)
    done : BOOLEAN;
    
    (* input vars *)
    p    : LRVector.T;
    func : LRScalarField.T;

    (* quit var *)
    doQuit : BOOLEAN;

    (* output var *)
    res    : LONGREAL;
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

      IF FALSE THEN Debug.Out("Robust.m3 : LinMinApply : done FALSE.") END;

      (* NOT cl.done *)
      IF cl.doQuit THEN
        EXIT
      END;
        
      LOCK mu DO INC(running) END;
      
      WITH result = cl.func.eval(cl.p) DO
        Debug.Out("Result " & LR(result));
        LOCK mu DO
          cl.res := result;
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
                p   : LRVector.T;
                func : LRScalarField.T) =
  BEGIN
    LOCK mu DO
      <*ASSERT t.done = TRUE*>
      t.p := p;
      t.func := func;
      t.done := FALSE;
      Thread.Signal(t.c) 
    END
  END Start;

PROCEDURE Wait(t : T) : LONGREAL =
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
