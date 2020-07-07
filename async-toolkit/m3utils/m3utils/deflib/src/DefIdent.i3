INTERFACE DefIdent;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;

TYPE T = TEXT;

PROCEDURE Get(p : RecursiveParser.T; VAR t : T) : BOOLEAN;

PROCEDURE MustBe(p : RecursiveParser.T; VAR t : T) RAISES { E };

CONST Brand = "DefIdent";

END DefIdent.
