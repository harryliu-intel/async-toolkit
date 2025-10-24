MODULE PicPoint;
IMPORT Word;
IMPORT PicCoord;
IMPORT PicExtent;
FROM Fmt IMPORT F;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Times(Word.Plus(PicCoord.Hash(a.x), 17), PicCoord.Hash(a.y))
  END Hash;
  
PROCEDURE Extent(READONLY a : T) : PicExtent.T =
  BEGIN RETURN PicExtent.T { a, a } END Extent;

PROCEDURE Minus(READONLY a, b : T) : T =
  BEGIN
    RETURN T { a.x - b.x, a.y - b.y }
  END Minus;

PROCEDURE Plus(READONLY a, b : T) : T =
  BEGIN
    RETURN T { a.x + b.x, a.y + b.y }
  END Plus;

PROCEDURE Times(a : LONGREAL; READONLY v : T) : T =
  BEGIN
    RETURN T { a * v.x, a * v.y }
  END Times;

PROCEDURE Translate(READONLY a : T; READONLY by : T) : T =
  BEGIN
    RETURN Plus(a, by)
  END Translate;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("%s { %s , %s }",
             Brand,
             PicCoord.Format(a.x),
             PicCoord.Format(a.y))
  END Format;
  
BEGIN END PicPoint.
