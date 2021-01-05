MODULE PicSegment;
IMPORT Word;
IMPORT PicPoint;
IMPORT PicExtent;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Times(Word.Plus(PicPoint.Hash(a.a), 17),
                      Word.Plus(PicPoint.Hash(a.b), 13))
  END Hash;
  
PROCEDURE Extent(READONLY a : T) : PicExtent.T =
  BEGIN
    RETURN PicExtent.T { PicPoint.T { MIN(a.a.x, a.b.x),
                                      MIN(a.a.y, a.b.y) },
                         PicPoint.T { MAX(a.a.x, a.b.x),
                                      MAX(a.a.y, a.b.y) } }
  END Extent;

BEGIN END PicSegment.
