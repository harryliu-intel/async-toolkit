(* $Id$ *)

MODULE SchemeLongReal;
IMPORT Scan;
FROM SchemeUtils IMPORT Error, DebugFormat;
FROM Scheme IMPORT Object;

PROCEDURE FromLR(x : LONGREAL) : T =
  BEGIN
    WITH new = NEW(T) DO
      new^ := x;
      RETURN new
    END
  END FromLR;

PROCEDURE FromO(x : Object) : T =
  BEGIN
    IF ISTYPE(x,T) THEN RETURN x 
    ELSE RETURN FromO(Error("expected a double, got: " & DebugFormat(x)))
    END
  END FromO;

PROCEDURE FromT(t : TEXT) : T =
  BEGIN RETURN FromLR(Scan.LongReal(t)) END FromT;

BEGIN END SchemeLongReal.

