MODULE PicSegment;
IMPORT Word;
IMPORT PicPoint;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Times(Word.Plus(PicPoint.Hash(a.a), 17),
                      Word.Plus(PicPoint.Hash(a.b), 13))
  END Hash;

BEGIN END PicSegment.
