MODULE PointMetric;
IMPORT LongrealType;
FROM Fmt IMPORT F, LongReal;
FROM GenOptUtils IMPORT FmtP;
IMPORT MultiEval;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN
    RETURN LongrealType.Compare(a.metric, b.metric)
  END Compare;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("{ metric %s ; p %s ; result %s }",
             LongReal(a.metric),
             FmtP(a.p),
             MultiEval.Format(a.result))
  END Format;

BEGIN END PointMetric.
