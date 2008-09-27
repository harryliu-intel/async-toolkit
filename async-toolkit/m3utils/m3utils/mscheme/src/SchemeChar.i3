(* $Id$ *)

INTERFACE SchemeChar;
IMPORT SchemeObject;

TYPE T = REF CHAR;

PROCEDURE Char(x : SchemeObject.T) : CHAR;
  
PROCEDURE Character(c : CHAR) : T;

CONST Brand = "SchemeChar";

END SchemeChar.
