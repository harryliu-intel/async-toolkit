(* $Id$ *)

INTERFACE LRVector;

TYPE T = REF ARRAY OF LONGREAL;

CONST Brand = "LRVector";

PROCEDURE Norm(v : T) : LONGREAL;

END LRVector.
