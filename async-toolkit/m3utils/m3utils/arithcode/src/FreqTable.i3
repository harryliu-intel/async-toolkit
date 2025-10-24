INTERFACE FreqTable;
IMPORT ArithProbability;
IMPORT ArithBits AS Bits;
IMPORT Rd;
IMPORT Thread;

TYPE
  T   = ARRAY [ ORD(FIRST(CHAR)) .. ORD(LAST(CHAR)) + 1 ] OF Bits.Freq;
  (* 0 .. 256 *)

  Cum = ARRAY [ FIRST(T) .. LAST(T) + 1 ] OF Bits.Freq;
  (* 0 .. 257 *)
  (* last position holds the total count *)

PROCEDURE GetProbability(READONLY cum : Cum; c : CHAR) : ArithProbability.T;

PROCEDURE Accumulate(READONLY t : T; VAR cum : Cum);
  (* zeros and accumulates a T *)
  
CONST Brand = "FreqTable";


  (* 
     the following two routines construct a frequency table from the 
     indicated data sources.

     If exactly zero counts of any given character are in the data source, 
     the result will contain zero for that character.  Otherwise, the result
     will be at least 1 for the character.

     Note that the count for EOF will always be one.
  *)

PROCEDURE FromRd(rd : Rd.T) : T RAISES { Rd.Failure, Thread.Alerted };

PROCEDURE FromText(txt : TEXT) : T;

PROCEDURE Unzero(VAR t : T);
  (* increase each t to at least 1 *)
  
END FreqTable.
