INTERFACE WordUtils;
IMPORT Word;

TYPE
  T = Word.T;
  Bits = ARRAY OF BOOLEAN;

PROCEDURE FromBits(READONLY bits: Bits): T;

END WordUtils.
