MODULE PicPoint;
IMPORT Word;
IMPORT PicCoord;
IMPORT PicExtent;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Times(Word.Plus(PicCoord.Hash(a.x), 17), PicCoord.Hash(a.y))
  END Hash;
  
PROCEDURE Extent(READONLY a : T) : PicExtent.T =
  BEGIN RETURN PicExtent.T { a, a } END Extent;

BEGIN END PicPoint.
