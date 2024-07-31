INTERFACE Entry;
IMPORT TextSeq;

(* this is a point in a Sweep 

   x is the independent value (parameter)

   report is all the other values, in text format
*)

TYPE
  T = RECORD
    x      : LONGREAL;
    report : TextSeq.T;
  END;

PROCEDURE CompareByX(READONLY a, b : T) : [-1..1];

CONST Compare = CompareByX;

CONST Brand = "Entry";

END Entry.
  
