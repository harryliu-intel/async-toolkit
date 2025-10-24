MODULE PicCircle;

IMPORT PicExtent;
IMPORT PicPoint;
FROM Fmt IMPORT F;
IMPORT PicCoord;

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
  
PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("%s { %s , %s }",
             Brand,
             PicPoint.Format(a.at),
             PicCoord.Format(a.r))
  END Format;

BEGIN END PicCircle.
