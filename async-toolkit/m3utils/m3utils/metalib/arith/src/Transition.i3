INTERFACE Transition;
IMPORT Refany, Word;

TYPE  T <: ROOT;

CONST Brand = "Transition";
      
CONST Equal = Refany.Equal;

PROCEDURE Hash(a : T) : Word.T;

CONST Compare : PROCEDURE(a, b : T) : [-1..1] = NIL;

END Transition.
