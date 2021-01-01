MODULE PicPoint;
IMPORT Word, Integer;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Times(Word.Plus(Integer.Hash(a.x), 17), Integer.Hash(a.y))
  END Hash;

BEGIN END PicPoint.
