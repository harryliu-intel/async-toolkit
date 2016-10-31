INTERFACE Range;
IMPORT SBAddress AS Address;
IMPORT Word;

TYPE
  B = RECORD lo, len : Address.T; group : TEXT END;
  T = REF B;

CONST Brand = "Range";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE CanMerge(READONLY a, b : T; VAR c : T) : BOOLEAN;

PROCEDURE Overlap(READONLY a, b : T) : BOOLEAN;

PROCEDURE Format(READONLY a : T) : TEXT;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Compare(READONLY a, b : T) : [-1..1];
  
END Range.
