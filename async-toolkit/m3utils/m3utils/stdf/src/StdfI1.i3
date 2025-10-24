INTERFACE StdfI1;
IMPORT Rd, StdfE;
IMPORT Word;
IMPORT Thread;
IMPORT Wr;
IMPORT StdfConstProc;

CONST Bytez = 1;
      Bits  = Bytez * 8;
      
TYPE T = [ -Word.Shift(1,Bits-1) .. Word.Shift(1,Bits-1)-1 ];

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;

PROCEDURE Bytes(READONLY t : T) : CARDINAL;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Wr.Failure, Thread.Alerted };
  
CONST Brand = "StdfI1";

END StdfI1.
