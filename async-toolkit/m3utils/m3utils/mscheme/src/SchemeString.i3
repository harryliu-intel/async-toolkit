(* $Id$ *)

INTERFACE SchemeString;

TYPE T = REF ARRAY OF CHAR;

CONST Brand = "SchemeString";

PROCEDURE FromText(txt : TEXT) : T;

END SchemeString.
