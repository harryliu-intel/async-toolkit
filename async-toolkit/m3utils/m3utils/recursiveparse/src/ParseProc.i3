INTERFACE ParseProc; (* could be made generic so we can specialize the type *)
IMPORT RecursiveParser;
IMPORT ParseError;

TYPE T = PROCEDURE(t              : RecursiveParser.T;
                   context        : REFANY)
  RAISES { ParseError.E };

CONST Brand = "ParseProc";

END ParseProc.
