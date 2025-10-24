(* $Id: Matrix2RType.i3,v 1.2 2008/02/04 00:10:46 mika Exp $ *)

INTERFACE Matrix2RType;
IMPORT Random;

TYPE T = REAL;

PROCEDURE Format(t : T) : TEXT;
PROCEDURE Rand(r : Random.T) : T;

CONST Brand = "Matrix2RType";

END Matrix2RType.
