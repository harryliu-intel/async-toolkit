INTERFACE StdfN1;
IMPORT Rd, StdfE;
IMPORT Thread;

TYPE T = [0..15];

PROCEDURE ParseArray(rd : Rd.T; VAR len : CARDINAL; VAR t : ARRAY OF T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfN1";

END StdfN1.
