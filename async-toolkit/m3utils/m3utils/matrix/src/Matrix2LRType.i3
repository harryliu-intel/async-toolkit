(* $Id: Matrix2LRType.i3,v 1.2 2008/02/04 00:10:46 mika Exp $ *)

INTERFACE Matrix2LRType;
IMPORT Random;

TYPE T = LONGREAL;

PROCEDURE Format(t : T) : TEXT;
PROCEDURE Rand(r : Random.T) : T;

CONST Brand = "Matrix2LRType";

END Matrix2LRType.
