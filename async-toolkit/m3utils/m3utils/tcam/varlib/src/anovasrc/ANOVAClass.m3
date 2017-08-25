(* $Id: ANOVAClass.m3,v 1.1 2006/03/06 02:29:02 mika Exp $ *)

MODULE ANOVAClass;
IMPORT Math;

PROCEDURE UnitVector(sz : CARDINAL) : RarrL =
  VAR
    res := NEW(RarrL, sz);
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      res[i] := 1.0d0 
    END;

    RETURN res
  END UnitVector;

PROCEDURE Normalize(VAR a : ARRAY OF LONGREAL) =
  VAR
    sumsq := 0.0d0;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      sumsq := sumsq + a[i]*a[i]
    END;
    VAR
      k := 1.0d0/Math.sqrt(sumsq);
    BEGIN
      FOR i := FIRST(a) TO LAST(a) DO
        a[i] := a[i]*k
      END
    END
  END Normalize;

PROCEDURE ComponentMul(READONLY a, b : ARRAY OF LONGREAL;
                       VAR r : ARRAY OF LONGREAL;
                       scalar := 1.0d0) =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      r[i] := a[i] * b[i] * scalar
    END
  END ComponentMul;

PROCEDURE Dot(READONLY a, b : ARRAY OF LONGREAL) : LONGREAL =
  VAR
    res := 0.0d0;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      res := res + a[i]*b[i]
    END;
    RETURN res
  END Dot;

BEGIN END ANOVAClass.
