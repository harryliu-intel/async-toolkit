INTERFACE NodePair;
IMPORT Name, Dsim, Word;

TYPE T = RECORD fanout, fanin : Name.T; outDir, inDir : Dsim.Sense END;
     
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Format(t : T) : TEXT;

CONST Brand = "NodePair";

END NodePair.
