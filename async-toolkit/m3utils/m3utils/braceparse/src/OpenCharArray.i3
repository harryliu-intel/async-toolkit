INTERFACE OpenCharArray;
IMPORT Word;

TYPE T = ARRAY OF CHAR;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

CONST Brand = "OpenCharArray";

PROCEDURE Clone(READONLY a : T) : REF T;

END OpenCharArray.
