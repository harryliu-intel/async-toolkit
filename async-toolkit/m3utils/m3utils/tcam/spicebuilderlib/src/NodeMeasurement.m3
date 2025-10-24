MODULE NodeMeasurement;
IMPORT Word;
IMPORT Text;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Plus(Text.Hash(a.nm), ORD(a.quantity))
  END Hash;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN Text.Equal(a.nm, b.nm) AND a.quantity = b.quantity
  END Equal;

BEGIN END NodeMeasurement.
