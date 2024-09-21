MODULE PointMetric;
IMPORT LongrealType;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN
    RETURN LongrealType.Compare(a.metric, b.metric)
  END Compare;

BEGIN END PointMetric.
