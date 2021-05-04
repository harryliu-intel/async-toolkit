INTERFACE LrArray;
IMPORT Word;

TYPE T = REF ARRAY OF LONGREAL;

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

CONST Brand = "LrArray";

END LrArray.
