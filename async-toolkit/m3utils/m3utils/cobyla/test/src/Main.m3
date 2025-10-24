MODULE Main;

(* test of COBYLA *)

IMPORT COBYLA_M3 AS COBYLA;
IMPORT LRVectorField;
IMPORT LRVector;
IMPORT Debug;
FROM Fmt IMPORT LongReal, Int;
IMPORT Fmt;

CONST LR = LongReal;

TYPE
  F = LRVectorField.T OBJECT
    res : LRVector.T;
  OVERRIDES
    eval := Eval;
  END;

PROCEDURE Eval(f : F; q : LRVector.T) : LRVector.T =
  
  BEGIN
    WITH x = q[0], y = q[1] DO
      f.res[0] := -(x + y);
      f.res[1] := y - (1.0d0 - x);      (* must be above line y = 1 - x *)
      f.res[2] := 1.0d0 - x * x - y * y; (* must be inside unit circle *)

      Debug.Out(Fmt.F("x=%s y=%s f[0]=%s f[1]=%s f[2]=%s",
                      LR(x), LR(y), LR(f.res[0]), LR(f.res[1]), LR(f.res[2])))
    END;
    RETURN f.res
  END Eval;
  
VAR
  p := NEW(LRVector.T, 2);
  f : F;
  m := 2;
BEGIN

  p^ := LRVector.S { 0.0d0, 1.0d0 };
  f  := NEW(F, res := NEW(LRVector.T, m + 1));
  WITH res = COBYLA.Minimize(p,
                             m,
                             f,
                             1.0d-1,
                             1.0d-6,
                             1000,
                             iprint := 1) DO
    Debug.Out(Fmt.F("F = %s", LR(res.f)));
    FOR i := FIRST(res.con^) TO LAST(res.con^) DO
      Debug.Out(Fmt.F("con[%s] = %s", Int(i), LR(res.con[i])))
    END
  END
                  
END Main.
