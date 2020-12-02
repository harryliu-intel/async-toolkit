INTERFACE StdfU4;
IMPORT Rd, StdfE;
IMPORT Word;
IMPORT Thread;
IMPORT Wr;
IMPORT StdfConstProc;

CONST Bytez = 4;
      Bits  = Bytez * 8;
      
TYPE T = [ 0 .. Word.Shift(1, Bits) - 1];

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure };

CONST Bytes = StdfConstProc.P4;

CONST Brand = "StdfU4";

END StdfU4.
