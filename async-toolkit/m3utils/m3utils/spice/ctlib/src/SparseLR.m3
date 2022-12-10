MODULE SparseLR;
IMPORT Word;
IMPORT Integer;
IMPORT LongrealType;

PROCEDURE CompareAbs(READONLY a, b : T) : [-1 .. 1] =
  BEGIN
    RETURN LongrealType.Compare(ABS(a.val), ABS(b.val))
  END CompareAbs;

PROCEDURE CompareVal(READONLY a, b : T) : [-1 .. 1] =
  BEGIN
    RETURN LongrealType.Compare(a.val, b.val)
  END CompareVal;

PROCEDURE CompareIdx(READONLY a, b : T) : [-1 .. 1] =
  BEGIN
    RETURN Integer.Compare(a.idx, b.idx)
  END CompareIdx;

PROCEDURE NegCompareAbs(READONLY a, b : T) : [-1 .. 1] =
  BEGIN
    RETURN LongrealType.Compare(ABS(b.val), ABS(a.val))
  END NegCompareAbs;

PROCEDURE NegCompareAbsWt(READONLY a, b : T) : [-1 .. 1] =
  VAR
    ai := a.idx;
    bi := b.idx;
  BEGIN
    ai := Word.Or(ai, Word.RightShift(ai,  1));
    ai := Word.Or(ai, Word.RightShift(ai,  2));
    ai := Word.Or(ai, Word.RightShift(ai,  4));
    ai := Word.Or(ai, Word.RightShift(ai,  8));
    ai := Word.Or(ai, Word.RightShift(ai, 16));
    ai := Word.Or(ai, Word.RightShift(ai, 32));
    
    bi := Word.Or(bi, Word.RightShift(bi,  1));
    bi := Word.Or(bi, Word.RightShift(bi,  2));
    bi := Word.Or(bi, Word.RightShift(bi,  4));
    bi := Word.Or(bi, Word.RightShift(bi,  8));
    bi := Word.Or(bi, Word.RightShift(bi, 16));
    bi := Word.Or(bi, Word.RightShift(bi, 32));
    
    RETURN LongrealType.Compare(ABS(b.val) / FLOAT(bi, LONGREAL),
                                ABS(a.val) / FLOAT(ai, LONGREAL))
  END NegCompareAbsWt;

PROCEDURE NegCompareVal(READONLY a, b : T) : [-1 .. 1] =
  BEGIN
    RETURN LongrealType.Compare(b.val, a.val)
  END NegCompareVal;

PROCEDURE NegCompareIdx(READONLY a, b : T) : [-1 .. 1] =
  BEGIN
    RETURN Integer.Compare(b.idx, a.idx)
  END NegCompareIdx;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN a = b
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Plus(LongrealType.Hash(a.val), a.idx)
  END Hash;

BEGIN END SparseLR.
