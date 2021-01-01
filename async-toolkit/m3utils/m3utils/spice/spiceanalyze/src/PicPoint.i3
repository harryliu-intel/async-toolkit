INTERFACE PicPoint;
IMPORT Word;

TYPE
  T = RECORD x, y : INTEGER END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

CONST Brand = "PicPoint";

END PicPoint.
