INTERFACE PicCircle;
IMPORT PicPoint, PicCoord;

TYPE
  T = RECORD
    at : PicPoint.T;
    r  : PicCoord.NonNeg;
  END;

CONST Brand = "PicCircle";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END PicCircle.
