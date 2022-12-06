INTERFACE FreqTable;
IMPORT ArithProbability;
IMPORT ArithBits AS Bits;

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

END FreqTable.
