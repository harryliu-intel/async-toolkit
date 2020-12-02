INTERFACE StdfB1;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Wr;
IMPORT StdfConstProc;

TYPE T = ARRAY [0..8-1] OF BOOLEAN;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;
  
CONST Bytes = StdfConstProc.P1;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Wr.Failure, Thread.Alerted };
  
CONST Brand = "StdfB1";

END StdfB1.
