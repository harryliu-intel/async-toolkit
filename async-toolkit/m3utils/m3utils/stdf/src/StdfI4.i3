INTERFACE StdfI4;
IMPORT Rd, StdfE;
IMPORT Word;
IMPORT Thread;

CONST Bytes = 2;
      Bits  = Bytes * 8;
      
TYPE T = [-Word.Shift(1,Bits-1)..Word.Shift(1,Bits-1)-1];

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfI4";

END StdfI4.
