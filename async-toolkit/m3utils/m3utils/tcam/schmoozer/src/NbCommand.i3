INTERFACE NbCommand;
IMPORT Time;

TYPE
  T = OBJECT
    id   : CARDINAL;
    cmd  : TEXT;
    nbId : [-1..LAST(CARDINAL)] := -1;
    started : Time.T;
  END;

CONST Brand = "NbCommand";

END NbCommand.
