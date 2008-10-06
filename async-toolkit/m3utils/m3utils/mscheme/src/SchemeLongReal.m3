(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeLongReal;
IMPORT Scan;
FROM SchemeUtils IMPORT Error, DebugFormat;
FROM Scheme IMPORT Object, E;
IMPORT Lex, FloatMode;

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

PROCEDURE FromO(x : Object) : LONGREAL RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x,T) THEN RETURN NARROW(x,T)^ 
    ELSE RETURN FromO(Error("expected a double, got: " & DebugFormat(x)))
    END
  END FromO;

PROCEDURE FromT(t : TEXT) : T RAISES { E }=
  BEGIN 
    TRY
      RETURN FromLR(Scan.LongReal(t)) 
    EXCEPT
      FloatMode.Trap, Lex.Error =>
      RETURN Error("Not a number: " & t) 
    END
  END FromT;

BEGIN 
  Zero := NEW(T);
  One := NEW(T);
  Zero^ := 0.0d0;
  One^  := 1.0d0;
END SchemeLongReal.




