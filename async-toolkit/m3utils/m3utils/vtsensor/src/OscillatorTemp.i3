INTERFACE OscillatorTemp;
IMPORT Spline;
IMPORT DataPoint;

TYPE
  T = OBJECT
    temp : LONGREAL;
    data : REF ARRAY OF DataPoint.T; (* sorted by V *)
    curve : Spline.T;
    minV, maxV : LONGREAL;
  END;

CONST Brand = "OscillatorTemp";

END OscillatorTemp.
