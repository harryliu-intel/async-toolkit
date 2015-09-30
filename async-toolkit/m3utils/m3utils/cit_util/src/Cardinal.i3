(* $Id: Cardinal.i3,v 1.1 2001/11/26 22:40:00 mika Exp $ *)

INTERFACE Cardinal;
IMPORT Word;

CONST Brand = "Cardinal";
TYPE T = CARDINAL;
TYPE CompRet = [-1..1];

PROCEDURE Compare(a, b : T) : CompRet;
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

END Cardinal.
