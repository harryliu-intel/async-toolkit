INTERFACE StdfR4;
IMPORT Rd, StdfE;
IMPORT Thread;

CONST Bytes = 4;
      Bits  = Bytes * 8;
      
TYPE T = REAL;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;

CONST Brand = "StdfR4";

END StdfR4.
