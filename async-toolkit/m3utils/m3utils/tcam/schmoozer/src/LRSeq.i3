INTERFACE LRSeq;
IMPORT LongRealSeq;
IMPORT Word;

TYPE T = LongRealSeq.T;

CONST Brand = "LRSeq";

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE Equal(a, b : T) : BOOLEAN;

END LRSeq.
