INTERFACE Triangle;
IMPORT Word;

TYPE
  Arr = ARRAY [0..2] OF CARDINAL;

  T = OBJECT 
    s : Arr;
    neighbors := ARRAY [0..2] OF T { NIL, .. };
  END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

CONST Brand = "Triangle";

END Triangle.
