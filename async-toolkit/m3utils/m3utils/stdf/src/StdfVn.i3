INTERFACE StdfVn;
IMPORT Rd, StdfE;
IMPORT Thread;

TYPE T = REF ARRAY OF CHAR;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfVn";

END StdfVn.
