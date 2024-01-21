MODULE Entry;
IMPORT LongrealType;

PROCEDURE CompareByX(READONLY a, b : T) : [-1..1] =
  BEGIN
    RETURN LongrealType.Compare(a.x, b.x)
  END CompareByX;

BEGIN END Entry.
