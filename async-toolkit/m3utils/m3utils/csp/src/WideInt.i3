INTERFACE WideInt;
IMPORT Word;

TYPE T = ARRAY OF Word.T; (* 2s complement *)

PROCEDURE Format(READONLY a : T; base : CARDINAL) : TEXT;
  
END WideInt.
