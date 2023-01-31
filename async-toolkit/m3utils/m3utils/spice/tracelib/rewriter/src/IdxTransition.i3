INTERFACE IdxTransition;

TYPE
  T = RECORD
    idx : CARDINAL;
    tr  : Transition.T;
  END;

CONST Brand = "IdxTransition";

TYPE CompareResult = [-1 .. 1];
     
PROCEDURE CompareByIdx(READONLY a, b : T) : CompareResult;
PROCEDURE CompareByTime(READONLY a, b : T) : CompareResult;
PROCEDURE CompareBySlew(READONLY a, b : T) : CompareResult;
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

END IdxTransition.
