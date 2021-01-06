INTERFACE PowerScaling;
IMPORT TvpMeasurement, Power3;

PROCEDURE Predict(measAct    : TvpMeasurement.T;
                  measLkg    : TvpMeasurement.T;
                  reqV, reqT : LONGREAL;
                  fixedDynP  : LONGREAL
  ) : Power3.T;

CONST Brand = "PowerScaling";

END PowerScaling.
