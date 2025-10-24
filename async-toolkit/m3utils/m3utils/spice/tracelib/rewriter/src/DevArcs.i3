INTERFACE DevArcs;
IMPORT Word;

TYPE
  T = RECORD
    pfx, sfx : TEXT;
  END;

CONST Brand = "DevNodes";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

END DevArcs.
