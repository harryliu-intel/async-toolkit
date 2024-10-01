MODULE PointResult;
FROM GenOpt IMPORT FmtP;
FROM Fmt IMPORT F, LongReal, Bool;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("{ %s ; metric %s ; quadratic %s }",
             FmtP(a.p), LongReal(a.metric), Bool(a.quadratic))
  END Format;

BEGIN END PointResult.
