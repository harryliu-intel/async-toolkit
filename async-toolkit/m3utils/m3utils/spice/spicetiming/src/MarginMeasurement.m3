MODULE MarginMeasurement;
IMPORT LongrealType;
FROM Fmt IMPORT F, LongReal;
IMPORT MarginScenario;

PROCEDURE Compare(READONLY a, b : T) : [-1 .. 1] =
  BEGIN
    RETURN LongrealType.Compare(a.margin, b.margin)
  END Compare;

PROCEDURE Format(a : T) : TEXT =
  BEGIN
    RETURN F("%s margin=%s at=%s",
             MarginScenario.Format(a.scenario),
             LongReal(a.margin),
             LongReal(a.at))
  END Format;
  
BEGIN END MarginMeasurement.
