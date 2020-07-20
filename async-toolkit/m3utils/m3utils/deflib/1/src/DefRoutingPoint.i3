INTERFACE DefRoutingPoint;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;
IMPORT DefInt, DefCard;

TYPE
  T = RECORD
    x, y     : DefInt.T;
    extValue : DefCard.T := 0; 
  END;

CONST
  Asterisk = LAST(INTEGER);
  
  Initial = T { Asterisk, Asterisk, 0 };

CONST Brand = "DefRoutingPoint";


PROCEDURE MustBe(t : RecursiveParser.T; VAR p : T) RAISES { E };
PROCEDURE MustGet(t : RecursiveParser.T) : T RAISES { E };
PROCEDURE Get(t : RecursiveParser.T; VAR p : T) : BOOLEAN RAISES { E };
  (* a bit of a hack because of the LL(1) capability here *)

END DefRoutingPoint.

  
