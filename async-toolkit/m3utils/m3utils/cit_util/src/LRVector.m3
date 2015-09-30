(* $Id: LRVector.m3,v 1.1 2001/10/24 18:27:38 penzes Exp $ *)

MODULE LRVector;
IMPORT Math;

PROCEDURE Norm(v : T) : LONGREAL =
  VAR
    res := 0.0d0;
  BEGIN
    FOR i := FIRST(v^) TO LAST(v^) DO
      res := res + v[i]*v[i]
    END;
    RETURN Math.sqrt(res)
  END Norm;

BEGIN END LRVector.
