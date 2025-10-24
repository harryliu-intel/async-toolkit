INTERFACE Comp;

TYPE
  T = RECORD
    nm  : TEXT;
    val : LONGREAL;
    m   : CARDINAL := 1; (* multiplicity *)
  END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Compare(READONLY a, b : T) : [-1..1];
  (* compare by val *)

PROCEDURE CompareByMulVal(READONLY a, b : T) : [-1..1];
  (* compare by mult*val *)
  
CONST Brand = "Comp";

END Comp.
  
