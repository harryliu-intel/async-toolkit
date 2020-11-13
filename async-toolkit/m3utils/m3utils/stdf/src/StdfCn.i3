INTERFACE StdfCn;
IMPORT Rd, StdfE;
IMPORT Word;

TYPE T = REF ARRAY OF CHAR;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T) RAISES { StdfE.E };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfU1";

END StdfCn.
