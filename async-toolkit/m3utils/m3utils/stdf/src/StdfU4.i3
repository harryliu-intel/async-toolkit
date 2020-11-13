INTERFACE StdfU4;
IMPORT Rd, StdfE;
IMPORT Word;

CONST Bytes = 4;
      Bits  = Bytes * 8;
      
TYPE T = [0..Word.Shift(1,Bits)-1];

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T) RAISES { StdfE.E };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfU4";

END StdfU4.
