INTERFACE SpiceFormat;
IMPORT Rd;
IMPORT SpiceCircuitList;
IMPORT TextSpiceCircuitTbl;

TYPE
  ErrorData = OBJECT
    lNo : CARDINAL;
  END;
  
  T = OBJECT 
    subCkts : TextSpiceCircuitTbl.T;
  END;
    
EXCEPTION Error(ErrorData);

PROCEDURE ParseSpice(rd : Rd.T) : T
  RAISES { Rd.EndOfFile, Error };

CONST Brand = "SpiceFormat";

END SpiceFormat.
