MODULE PicCircle;

IMPORT PicExtent, PicPoint;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Extent(READONLY a : T) : PicExtent.T =
  BEGIN
    RETURN PicExtent.T { PicPoint.T { a.at.x - a.r, a.at.y - a.r },
                         PicPoint.T { a.at.x + a.r, a.at.y + a.r } };
  END Extent;
  
BEGIN END PicCircle.
