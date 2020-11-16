INTERFACE StdfB1;
IMPORT Rd, StdfE;
IMPORT Thread;

TYPE T = ARRAY [0..8-1] OF BOOLEAN;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfB1";

END StdfB1.
