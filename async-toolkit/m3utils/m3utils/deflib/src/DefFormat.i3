INTERFACE DefFormat;
IMPORT Rd;

TYPE
  T <: Public;

  Public = OBJECT END;

PROCEDURE Parse(rd : Rd.T) : T;

CONST Brand = "DefFormat";

END DefFormat.
