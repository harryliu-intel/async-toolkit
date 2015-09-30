(* $Id: Hashable.m3,v 1.1 2011/02/04 14:20:06 mika Exp $ *)

MODULE Hashable;
IMPORT Word;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.hash() END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a.equal(b) END Equal;

BEGIN END Hashable.
