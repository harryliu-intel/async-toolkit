INTERFACE SpiceFormat;
IMPORT Rd, Thread;
IMPORT SpiceCircuit;
IMPORT TextSpiceCircuitTbl;
IMPORT SpiceError;
IMPORT Pathname;
IMPORT TextSeq;

TYPE
  T = OBJECT
    topCkt      : SpiceCircuit.T;
    subCkts     : TextSpiceCircuitTbl.T;
    subCktNames : TextSeq.T; (* in correct sequence *)
  END;
    
PROCEDURE ParseSpice(rd                 : Rd.T;
                     currentSearchDir,
                     fn                 : Pathname.T) : T
  RAISES { Rd.Failure, SpiceError.E };

CONST Brand = "SpiceFormat";

PROCEDURE GetLine(rd : Rd.T;
                  VAR buff : REF ARRAY OF CHAR;
                  VAR lNo : CARDINAL) : [-1..LAST(CARDINAL)]
  RAISES { Rd.Failure, Thread.Alerted };
  (* GetLine returns -1 on EOF;
     buff can be NIL or have zero size on call
     lNo should be initialized (line numbers will be relative therefrom)
  *)

END SpiceFormat.
