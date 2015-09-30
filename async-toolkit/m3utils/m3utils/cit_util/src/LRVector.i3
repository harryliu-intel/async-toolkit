(* $Id: LRVector.i3,v 1.3 2005/04/25 21:47:17 mika Exp $ *)

INTERFACE LRVector;

TYPE S = ARRAY OF LONGREAL;
TYPE T = REF S;

CONST Brand = "LRVector";

PROCEDURE Norm(v : T) : LONGREAL;

END LRVector.
