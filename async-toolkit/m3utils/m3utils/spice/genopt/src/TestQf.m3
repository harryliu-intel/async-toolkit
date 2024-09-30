MODULE TestQf;

IMPORT QuadraticFit;
IMPORT Debug;
IMPORT LRVector;
FROM Fmt IMPORT F, LongReal;
IMPORT LRMatrix2 AS M;
FROM GenOpt IMPORT FmtP;

CONST LR = LongReal;

PROCEDURE Func(p : LRVector.T) : LONGREAL =
  VAR
    res := 0.0d0;
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO
      WITH d = p[i] - 2.0d0 DO
        res := res + d * d
      END
    END;
    RETURN res
  END Func;

PROCEDURE DoIt() =
  VAR
    qf : QuadraticFit.T;

    p := NEW(LRVector.T, 1);
  BEGIN
    Debug.Out("TestQf.DoIt()");

    qf := NEW(QuadraticFit.T).init(NUMBER(p^), 1.0d0);

    M.ZeroV(p^);

    qf.addPoint(p, Func(p));

    (* eval at a buncha points *)
    FOR h := 1 TO 3 DO
      FOR i := FIRST(p^) TO LAST(p^) DO
        p[i] := 0.5d0 * FLOAT(h, LONGREAL);
        qf.addPoint(p, Func(p))
      END
    END;

    Debug.Out(F("TestQf : qf.getParams() = %s",
                FmtP(qf.getParams())))
    
  END DoIt;

BEGIN END TestQf.
