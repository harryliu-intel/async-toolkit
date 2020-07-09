INTERFACE DefString;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;

TYPE T = TEXT; (* note the clash with DefIdent.T *)

PROCEDURE Get(p : RecursiveParser.T; VAR t : T) : BOOLEAN;

PROCEDURE MustBe(p : RecursiveParser.T; VAR t : T) RAISES { E };
PROCEDURE MustGet(p : RecursiveParser.T) : T RAISES { E };

CONST Brand = "DefString";

END DefString.
