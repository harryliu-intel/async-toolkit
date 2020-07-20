INTERFACE ParseProc;
IMPORT DefFormat;
IMPORT ParseError;

TYPE T = PROCEDURE(t              : DefFormat.T;
                   context        : REFANY)
  RAISES { ParseError.E };

CONST Brand = "ParseProc";

END ParseProc.
