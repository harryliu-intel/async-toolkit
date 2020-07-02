INTERFACE DefFormat;
IMPORT Rd;
IMPORT ParseError;

TYPE
  T <: Public;

  Public = OBJECT END;

PROCEDURE Parse(rd : Rd.T) : T RAISES { ParseError.E };

CONST Brand = "DefFormat";

END DefFormat.
