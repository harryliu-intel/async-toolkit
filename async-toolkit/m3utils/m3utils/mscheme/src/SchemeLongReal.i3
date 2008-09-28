(* $Id$ *)

INTERFACE SchemeLongReal;
IMPORT SchemeObject;

TYPE T = REF LONGREAL;

PROCEDURE FromLR(x : LONGREAL) : T;

PROCEDURE FromO(x : SchemeObject.T) : T;

PROCEDURE FromT(t : TEXT) : T; (* my add'n *)

CONST Brand = "SchemeLongReal";

END SchemeLongReal.
