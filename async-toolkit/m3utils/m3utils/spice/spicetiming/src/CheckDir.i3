INTERFACE CheckDir;
IMPORT Transition;
IMPORT Word;

TYPE T = SET OF Transition.Dir;

CONST Brand = "CheckDir";

PROCEDURE Fmt(t : T) : TEXT;
CONST Format = Fmt;
      
PROCEDURE Hash(t : T) : Word.T;

PROCEDURE Equal(a, b : T) : BOOLEAN;
  
END CheckDir.
