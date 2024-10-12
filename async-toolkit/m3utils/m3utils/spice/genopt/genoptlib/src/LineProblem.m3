MODULE LineProblem;
IMPORT LongrealType;

PROCEDURE Compare(READONLY a, b : T) : [-1 .. 1] =
  BEGIN
    RETURN LongrealType.Compare(a.minval, b.minval)
  END Compare;

BEGIN END LineProblem.
  
