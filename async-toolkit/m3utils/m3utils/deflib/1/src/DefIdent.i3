INTERFACE DefIdent;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;

TYPE T = TEXT;

PROCEDURE Get(p : RecursiveParser.T; VAR t : T) : BOOLEAN;
PROCEDURE Peek(p : RecursiveParser.T; VAR t : T) : BOOLEAN;
PROCEDURE MustBe(p : RecursiveParser.T; VAR t : T) RAISES { E };
PROCEDURE MustGet(p : RecursiveParser.T) : T RAISES { E };

CONST Brand = "DefIdent";

END DefIdent.
