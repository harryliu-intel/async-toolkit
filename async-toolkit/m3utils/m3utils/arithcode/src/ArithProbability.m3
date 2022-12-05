MODULE ArithProbability;
FROM Fmt IMPORT Int, F;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN F("{lo=%s hi=%s cnt=%s}", Int(t.lo), Int(t.hi), Int(t.count))
  END Format;

BEGIN END ArithProbability.
