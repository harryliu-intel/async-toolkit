MODULE Scatter;
IMPORT MultiEvalLRVector, MultiEvalLR;
IMPORT PointMetricLRVector, PointMetricLR;
IMPORT MELRType, MELRVectorType;

PROCEDURE MultiEvalResult(READONLY in : MultiEvalLRVector.Result)
  : REF ARRAY OF MultiEvalLR.Result =
  VAR
    n   := NUMBER(in.sum^);
    res := NEW(REF ARRAY OF MultiEvalLR.Result, n);
  BEGIN
    FOR i := 0 TO n - 1 DO
      VAR
        nominal : LONGREAL;
      BEGIN
        IF in.nominal = MELRVectorType.Null THEN
          nominal := MELRType.Null
        ELSE
          nominal := in.nominal[i]
        END;
        res[i] := MultiEvalLR.Result {
        subdirPath := in.subdirPath,
        id      := in.id,
        nominal := nominal,
        n       := in.n,
        sum     := in.sum[i],
        sumsq   := in.sumsq[i],
        extra   := in.extra
        }
      END
    END;
    RETURN res
  END MultiEvalResult;

PROCEDURE PointMetric(READONLY in : PointMetricLRVector.T)
  : REF ARRAY OF PointMetricLR.T =
  VAR
    n      := NUMBER(in.result.sum^);
    result := MultiEvalResult(in.result);
    res    := NEW(REF ARRAY OF PointMetricLR.T, n);
  BEGIN
    FOR i := 0 TO n - 1 DO
      res[i] := PointMetricLR.T {
      metric     := in.metric,
      p          := in.p,
      result     := result[i]
      }
    END;
    RETURN res
  END PointMetric;

BEGIN END Scatter.
