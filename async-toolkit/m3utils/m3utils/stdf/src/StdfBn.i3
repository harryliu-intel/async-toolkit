INTERFACE StdfBn;
IMPORT Rd, StdfE;
IMPORT Thread;

TYPE T = REF ARRAY OF BOOLEAN;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.Missing, StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfB1";

END StdfBn.
