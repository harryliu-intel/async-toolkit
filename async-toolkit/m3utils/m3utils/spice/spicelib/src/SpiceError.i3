INTERFACE SpiceError;
IMPORT Pathname;

TYPE
  Data = RECORD
    msg : TEXT := "";
    lNo : CARDINAL := 0;
    fn  : Pathname.T := NIL;
  END;
  
EXCEPTION E(Data);

CONST Brand = "SpiceError";

END SpiceError.
