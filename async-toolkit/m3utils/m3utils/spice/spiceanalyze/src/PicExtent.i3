INTERFACE PicExtent;
IMPORT PicPoint, PicCoord;

TYPE
  T = PicPoint.ExtentT;

CONST
  Empty =   T { PicPoint.T { LAST(PicCoord.T), LAST(PicCoord.T) },
                PicPoint.T { FIRST(PicCoord.T), FIRST(PicCoord.T) } };

  Zero = T { PicPoint.Zero, PicPoint.Zero };

PROCEDURE Merge(READONLY a, b : T) : T;

CONST Brand = "PicExtent";

PROCEDURE Format(READONLY a : T) : TEXT;
  
END PicExtent.
  

