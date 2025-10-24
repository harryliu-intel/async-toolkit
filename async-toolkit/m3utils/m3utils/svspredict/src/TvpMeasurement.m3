MODULE TvpMeasurement;
FROM Fmt IMPORT F, LongReal;

CONST LR = LongReal;

PROCEDURE Fmt(READONLY q : T) : TEXT =
  BEGIN
    RETURN F("Tvp { t = %s, v = %s, p = %s }",
             LR(q.t), LR(q.v), LR(q.p))
  END Fmt;

BEGIN END TvpMeasurement.
