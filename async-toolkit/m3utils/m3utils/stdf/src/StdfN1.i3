INTERFACE StdfN1;
IMPORT Rd, StdfE;

TYPE T = [0..15];

PROCEDURE ParseArray(rd : Rd.T; VAR len : CARDINAL; VAR t : ARRAY OF T) RAISES { StdfE.E };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfN1";

END StdfN1.
