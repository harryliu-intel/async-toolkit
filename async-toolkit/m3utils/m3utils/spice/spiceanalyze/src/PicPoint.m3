MODULE PicPoint;
IMPORT Word;
IMPORT PicCoord;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Times(Word.Plus(PicCoord.Hash(a.x), 17), PicCoord.Hash(a.y))
  END Hash;

BEGIN END PicPoint.
