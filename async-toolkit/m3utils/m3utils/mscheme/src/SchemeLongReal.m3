(* $Id$ *)

MODULE SchemeLongReal;
IMPORT Scan;
FROM SchemeUtils IMPORT Error, DebugFormat;
FROM Scheme IMPORT Object;

PROCEDURE FromLR(x : LONGREAL) : T =
  BEGIN
    IF    x = 0.0d0 THEN RETURN Zero
    ELSIF x = 1.0d0 THEN RETURN One
    ELSE
      WITH new = NEW(T) DO
        new^ := x;
        RETURN new
      END
    END
  END FromLR;

PROCEDURE FromO(x : Object) : LONGREAL =
  BEGIN
    IF x # NIL AND ISTYPE(x,T) THEN RETURN NARROW(x,T)^ 
    ELSE RETURN FromO(Error("expected a double, got: " & DebugFormat(x)))
    END
  END FromO;

PROCEDURE FromT(t : TEXT) : T =
  BEGIN RETURN FromLR(Scan.LongReal(t)) END FromT;


BEGIN 
  Zero := NEW(T);
  One := NEW(T);
  Zero^ := 0.0d0;
  One^  := 1.0d0;
END SchemeLongReal.




