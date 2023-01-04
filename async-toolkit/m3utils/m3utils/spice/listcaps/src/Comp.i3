INTERFACE Comp;

TYPE
  T = RECORD
    nm  : TEXT;
    val : LONGREAL;
  END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Compare(READONLY a, b : T) : [-1..1]; (* compare by val *)

CONST Brand = "Comp";

END Comp.
  
