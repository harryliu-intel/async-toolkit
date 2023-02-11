INTERFACE FlatRule;
IMPORT Word;

TYPE
  T <: ROOT;

CONST Brand = "FlatRule";

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

END FlatRule.
