(* $Id: Matrix2XType.i3,v 1.2 2008/02/04 00:10:46 mika Exp $ *)

INTERFACE Matrix2XType;
IMPORT Random;

TYPE T = EXTENDED;

PROCEDURE Format(t : T) : TEXT;
PROCEDURE Rand(r : Random.T) : T;

CONST Brand = "Matrix2XType";

END Matrix2XType.
