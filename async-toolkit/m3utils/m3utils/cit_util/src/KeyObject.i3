INTERFACE KeyObject;
IMPORT Word;

TYPE
  T = OBJECT METHODS
    hash(): Word.T;
    equal(other: T): BOOLEAN;
  END;

PROCEDURE Hash(key: T): Word.T;
PROCEDURE Equal(k1, k2: T): BOOLEAN;

CONST
  Brand = "KeyObject";

END KeyObject.
