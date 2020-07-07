INTERFACE DefName;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;

TYPE T = RECORD END; (* TBD *)

PROCEDURE Get(t : RecursiveParser.T; VAR name : T) : BOOLEAN RAISES { E };
PROCEDURE MustBe(t : RecursiveParser.T; VAR name : T) RAISES { E };

CONST Brand = "DefName";

END DefName.
