(* $Id$ *)

INTERFACE LRVector;

TYPE S = ARRAY OF LONGREAL;
TYPE T = REF S;

CONST Brand = "LRVector";

PROCEDURE Norm(v : T) : LONGREAL;

END LRVector.
