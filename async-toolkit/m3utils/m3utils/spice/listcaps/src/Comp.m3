MODULE Comp;
IMPORT Text;
IMPORT LongrealType;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN Text.Equal(a.nm, b.nm) AND a.val = b.val
  END Equal;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN
    RETURN LongrealType.Compare(a.val, b.val)
  END Compare;

BEGIN END Comp.
