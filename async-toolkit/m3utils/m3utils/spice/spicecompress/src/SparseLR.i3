INTERFACE SparseLR;
IMPORT Word;

TYPE
  T = RECORD
    idx : CARDINAL;
    val : LONGREAL;
  END;

CONST Brand = "SparseLR";

PROCEDURE CompareAbs(READONLY a, b : T) : [-1 .. 1];

PROCEDURE CompareVal(READONLY a, b : T) : [-1 .. 1];

PROCEDURE CompareIdx(READONLY a, b : T) : [-1 .. 1];

PROCEDURE NegCompareAbs(READONLY a, b : T) : [-1 .. 1];

PROCEDURE NegCompareAbsWt(READONLY a, b : T) : [-1 .. 1];

PROCEDURE NegCompareVal(READONLY a, b : T) : [-1 .. 1];

PROCEDURE NegCompareIdx(READONLY a, b : T) : [-1 .. 1];

CONST Compare = CompareAbs;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;
      
END SparseLR.
