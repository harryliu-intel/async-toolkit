INTERFACE Entry;
IMPORT TextSeq;

TYPE
  T = RECORD
    x      : LONGREAL;
    report : TextSeq.T;
  END;

PROCEDURE CompareByX(READONLY a, b : T) : [-1..1];

CONST Compare = CompareByX;

CONST Brand = "Entry";

END Entry.
  
