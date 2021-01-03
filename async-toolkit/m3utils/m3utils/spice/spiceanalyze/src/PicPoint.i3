INTERFACE PicPoint;
IMPORT Word;
IMPORT PicCoord;

TYPE
  T = RECORD x, y : PicCoord.T END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

CONST Brand = "PicPoint";

END PicPoint.
