INTERFACE PicSegment;
IMPORT PicPoint;
IMPORT Word;

TYPE
  T = RECORD a, b : PicPoint.T; w : CARDINAL END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

CONST Brand = "PicSegment";

END PicSegment.
