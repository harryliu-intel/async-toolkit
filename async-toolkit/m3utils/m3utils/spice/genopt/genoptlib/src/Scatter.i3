INTERFACE Scatter;
IMPORT MultiEvalLRVector, MultiEvalLR;
IMPORT PointMetricLRVector, PointMetricLR;

PROCEDURE MultiEvalResult(READONLY in : MultiEvalLRVector.Result)
  : REF ARRAY OF MultiEvalLR.Result;

PROCEDURE PointMetric(READONLY in : PointMetricLRVector.T)
  : REF ARRAY OF PointMetricLR.T;

END Scatter.
