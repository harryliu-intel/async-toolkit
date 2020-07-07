INTERFACE DefFormat;
IMPORT Rd;
IMPORT ParseError;
IMPORT RecursiveParser;

TYPE
  T <: Public;

  Public = RecursiveParser.T OBJECT END;

PROCEDURE Parse(rd : Rd.T) : T RAISES { ParseError.E };

CONST Brand = "DefFormat";

END DefFormat.
