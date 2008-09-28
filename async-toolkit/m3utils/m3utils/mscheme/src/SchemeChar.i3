(* $Id$ *)

INTERFACE SchemeChar;
IMPORT SchemeObject;

TYPE T <: REFANY;

(* all chars here are SHARED, so don't overwrite them, eh? *)

PROCEDURE Char(x : SchemeObject.T) : CHAR;

PROCEDURE Chr(x : SchemeObject.T) : T;

PROCEDURE IChr(x : INTEGER) : T;
  
PROCEDURE Character(c : CHAR) : T;

CONST Brand = "SchemeChar";

END SchemeChar.
