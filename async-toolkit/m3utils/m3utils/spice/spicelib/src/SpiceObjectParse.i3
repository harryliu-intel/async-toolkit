INTERFACE SpiceObjectParse;
IMPORT SpiceCircuitList;
IMPORT TextSpiceCircuitTbl;

PROCEDURE ParseLine(VAR circuit : SpiceCircuitList.T; (* circuit stack *)
                    subCkts : TextSpiceCircuitTbl.T;
                    READONLY line : ARRAY OF CHAR;
                    lNo : CARDINAL (* for errors *));

END SpiceObjectParse.
