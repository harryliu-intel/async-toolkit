INTERFACE FmError;

TYPE
  Id = [0..255]; (* one byte *)
  
  T = RECORD
    codeName : TEXT;
    id       : Id;
    desc     : TEXT;
  END;

CONST Brand = "FmError";

END FmError.
