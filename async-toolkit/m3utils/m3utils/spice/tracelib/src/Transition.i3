INTERFACE Transition;
IMPORT Word;

TYPE
  Dir = [-1 .. 1];
  
  T = RECORD
    at       : LONGREAL;
    dir      : Dir;
    slew     := NoSlew; (* only filled in if doSlew is TRUE, always pos. *)
  END;

CONST NoSlew = FIRST(LONGREAL);
      
CONST Brand = "Transition";

TYPE CompareResult = [-1 .. 1];
     
PROCEDURE CompareByTime(READONLY a, b : T) : CompareResult;
PROCEDURE CompareBySlew(READONLY a, b : T) : CompareResult;

CONST Compare = CompareBySlew;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Format(READONLY a : T) : TEXT;

END Transition.
