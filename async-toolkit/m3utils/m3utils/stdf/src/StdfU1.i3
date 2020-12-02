INTERFACE StdfU1;
IMPORT Rd, StdfE;
IMPORT Word;
IMPORT Thread;
IMPORT Wr;
IMPORT StdfConstProc;

CONST Bytez = 1;
      Bits  = Bytez * 8;
      
TYPE T = [0..Word.Shift(1,Bits)-1];

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

CONST Bytes = StdfConstProc.P1;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure };

PROCEDURE Format(t : T) : TEXT;

CONST Brand = "StdfU1";

END StdfU1.
