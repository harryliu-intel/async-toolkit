INTERFACE VSeq;
IMPORT TextSeq, Word;

TYPE T = TextSeq.T;

PROCEDURE Equal(a, b : T): BOOLEAN;

PROCEDURE Compare(a, b : T) : [-1..1];

PROCEDURE Hash(a : T) : Word.T;

CONST Brand = "VSeq";

CONST Sub = TextSeq.Sub; (* for now *)

END VSeq. 
