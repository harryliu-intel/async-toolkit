INTERFACE PrsNode;
IMPORT Word;

TYPE
  T <: ROOT;

CONST Brand = "PrsNode";

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

END PrsNode.
      
  
