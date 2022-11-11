INTERFACE CheckDir;
IMPORT Transition;

TYPE T = SET OF Transition.Dir;

CONST Brand = "CheckDir";

PROCEDURE Fmt(t : T) : TEXT;
  
END CheckDir.
