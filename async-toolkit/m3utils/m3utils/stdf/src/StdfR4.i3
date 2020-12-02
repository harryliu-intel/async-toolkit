INTERFACE StdfR4;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT StdfConstProc;
IMPORT Wr;

CONST Bytez = 4;
      Bits  = Bytez * 8;
      
TYPE T = REAL;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;

CONST Bytes = StdfConstProc.P4;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure };

CONST Brand = "StdfR4";

END StdfR4.
