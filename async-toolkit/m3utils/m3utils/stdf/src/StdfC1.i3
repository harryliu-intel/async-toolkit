INTERFACE StdfC1;
IMPORT Rd, StdfE, Thread;
IMPORT StdfConstProc;
IMPORT StdfWr;

TYPE T = ARRAY [0..1-1] OF CHAR;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfC1";

CONST Bytes = StdfConstProc.P1;

CONST Write = StdfWr.Char;

END StdfC1.
