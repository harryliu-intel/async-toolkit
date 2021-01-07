INTERFACE PicSegment;
IMPORT PicPoint;
IMPORT Word;
IMPORT PicExtent;

TYPE
  T = RECORD a, b : PicPoint.T END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Extent(READONLY a : T) : PicExtent.T;

PROCEDURE Translate(READONLY a : T; READONLY by : PicPoint.T) : T;

PROCEDURE Format(READONLY a : T) : TEXT;

CONST Brand = "PicSegment";

END PicSegment.
