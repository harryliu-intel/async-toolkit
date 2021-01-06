MODULE PicCircle;

IMPORT PicExtent, PicPoint;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Extent(READONLY a : T) : PicExtent.T =
  BEGIN
    RETURN PicExtent.T { PicPoint.T { a.at.x - a.r, a.at.y - a.r },
                         PicPoint.T { a.at.x + a.r, a.at.y + a.r } };
  END Extent;
  
PROCEDURE Translate(READONLY a : T; READONLY by : PicPoint.T) : T =
  VAR
    res := a;
  BEGIN
    res.at := PicPoint.Plus(res.at, by);
    RETURN res
  END Translate;
  
BEGIN END PicCircle.
