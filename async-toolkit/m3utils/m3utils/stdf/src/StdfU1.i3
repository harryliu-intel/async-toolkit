INTERFACE StdfU1;
IMPORT Rd, StdfE;

TYPE T = [0..255];

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T) RAISES { StdfE.E };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = StdfU1;
