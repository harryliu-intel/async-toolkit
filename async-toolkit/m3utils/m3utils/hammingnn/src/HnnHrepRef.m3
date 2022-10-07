MODULE HnnHrepRef;
IMPORT Word;
IMPORT HnnHrep;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    RETURN HnnHrep.Equal(a^, b^)
  END Equal;
      
PROCEDURE Hash(a : T) : Word.T =
  BEGIN
    RETURN HnnHrep.Hash(a^)
  END Hash;

BEGIN END HnnHrepRef.
