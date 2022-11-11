MODULE MarginMeasurement;
IMPORT LongrealType;

PROCEDURE Compare(READONLY a, b : T) : [-1 .. 1] =
  BEGIN
    RETURN LongrealType.Compare(a.margin, b.margin)
  END Compare;

BEGIN END MarginMeasurement.
