(* $Id$ *)

INTERFACE SchemeLongReal;

TYPE T = REF LONGREAL;

PROCEDURE FromLR(x : LONGREAL) : T;

PROCEDURE FromO(x : REFANY) : T;

PROCEDURE FromT(t : TEXT) : T; (* my add'n *)


CONST Brand = "SchemeLongReal";

END SchemeLongReal.
