MODULE Main;
IMPORT Coroutine;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT RTCollector;

PROCEDURE P(arg : Coroutine.Arg) =
  BEGIN
    FOR i := 0 TO 19 DO
      Debug.Out(F("Hello %s from P!", Int(i)));

      RTCollector.Disable();
      Coroutine.Call(arg.co, arg.this);
      RTCollector.Enable();
    END;
  END P;

PROCEDURE Q(arg : Coroutine.Arg) =
  BEGIN
    FOR i := 0 TO 19 DO
      Debug.Out(F("Hello %s from Q!", Int(i)));
      Coroutine.Call(arg.co, arg.this)
    END;
  END Q;


BEGIN

  FOR i := 0 TO 9 DO
    Coroutine.CallPair(P, NIL, Q, NIL);
  END
  
END Main.
