INTERFACE Transition;

TYPE
  Dir = [-1 .. 1];
  
  T = RECORD
    at  : LONGREAL;
    dir : Dir;
  END;

CONST Brand = "Transition";

END Transition.
