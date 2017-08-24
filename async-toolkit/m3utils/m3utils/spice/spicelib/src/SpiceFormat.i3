INTERFACE SpiceFormat;
IMPORT Rd, Thread;
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

PROCEDURE GetLine(rd : Rd.T;
                  VAR buff : REF ARRAY OF CHAR;
                  VAR lNo : CARDINAL) : [-1..LAST(CARDINAL)] RAISES { Rd.Failure, Thread.Alerted };
  (* GetLine returns -1 on EOF;
     buff can be NIL or have zero size on call
     lNo should be initialized (line numbers will be relative therefrom)
  *)

END SpiceFormat.
