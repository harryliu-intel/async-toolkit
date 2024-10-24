GENERIC INTERFACE PointMetric(MultiEval);
IMPORT LRVector;

TYPE
  T = RECORD
    metric : LONGREAL;
    p      : LRVector.T;
    result : MultiEval.Result;
  END;

CONST Brand = "PointMetric(" & MultiEval.Brand & ")";

PROCEDURE Compare(READONLY a, b : T) : [-1..1];

PROCEDURE Format(READONLY a : T) : TEXT;

END PointMetric.
