INTERFACE PointMetric;
IMPORT LRVector;
IMPORT MultiEvalLR;

TYPE
  T = RECORD
    metric : LONGREAL;
    p      : LRVector.T;
    result : MultiEvalLR.Result;
  END;

CONST Brand = "PointMetric";

PROCEDURE Compare(READONLY a, b : T) : [-1..1];

PROCEDURE Format(READONLY a : T) : TEXT;

END PointMetric.
