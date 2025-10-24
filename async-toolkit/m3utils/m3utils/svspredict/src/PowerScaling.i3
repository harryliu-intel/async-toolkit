INTERFACE PowerScaling;
IMPORT TvpMeasurement, Power3;
FROM SvsTypes IMPORT CornerData;
IMPORT Power;

PROCEDURE Predict(measAct    : TvpMeasurement.T;
                  measLkg    : TvpMeasurement.T;
                  reqV, reqT : LONGREAL;
                  fixedDynP  : LONGREAL
  ) : Power3.T;

CONST Brand = "PowerScaling";

PROCEDURE Interpolate(READONLY p : Power.Params;
                      x : LONGREAL) : CornerData;


END PowerScaling.
