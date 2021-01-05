INTERFACE PicCircle;
IMPORT PicPoint, PicCoord;
IMPORT PicExtent;

TYPE
  T = RECORD
    at : PicPoint.T;
    r  : PicCoord.NonNeg;
  END;

CONST Brand = "PicCircle";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Extent(READONLY a : T) : PicExtent.T;

END PicCircle.
