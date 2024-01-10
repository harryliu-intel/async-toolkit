INTERFACE Conversion;
IMPORT Pathname;

TYPE
  T = RECORD
    ifn, ofn : Pathname.T;
  END;

CONST Brand = "Conversion";

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;
      
END Conversion.
