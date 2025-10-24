INTERFACE DefPoint;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;

TYPE
  T = RECORD
    x, y : INTEGER;
  END;

CONST Brand = "DefPoint";

PROCEDURE MustBe(t : RecursiveParser.T; VAR p : T) RAISES { E };
PROCEDURE MustGet(t : RecursiveParser.T) : T RAISES { E };
PROCEDURE Get(t : RecursiveParser.T; VAR p : T) : BOOLEAN RAISES { E };
  (* a bit of a hack because of the LL(1) capability here *)

END DefPoint.
