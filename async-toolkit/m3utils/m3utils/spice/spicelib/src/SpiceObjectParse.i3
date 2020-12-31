INTERFACE SpiceObjectParse;
IMPORT SpiceCircuitList;
IMPORT TextSpiceCircuitTbl;
IMPORT SpiceError;
IMPORT TextSeq;

PROCEDURE ParseLine(VAR circuit   : SpiceCircuitList.T; (* circuit stack *)
                    subCkts       : TextSpiceCircuitTbl.T;
                    subCktNames   : TextSeq.T;
                    READONLY line : ARRAY OF CHAR;
                    VAR warning   : TEXT)
  RAISES { SpiceError.E };

END SpiceObjectParse.
