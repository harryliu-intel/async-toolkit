INTERFACE LineProblem;
IMPORT LRVector;

TYPE
  T = RECORD
    dir    : LRVector.T;
    minp   : LRVector.T;
    minval : LONGREAL;
  END;

PROCEDURE Compare(READONLY a, b : T) : [-1 .. 1];

CONST Brand = "LineProblem";

END LineProblem.
  
