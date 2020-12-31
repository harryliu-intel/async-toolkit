INTERFACE SpiceObject;
IMPORT TextSeq;
IMPORT Wr;
IMPORT Word;
IMPORT Refany;

(* data structure describing hierarchical object *)

TYPE
  T <: Public;

  Public = BRANDED OBJECT
    name      : TEXT;
    terminals : TextSeq.T;
    data      : TextSeq.T;
  METHODS
    output(wr          : Wr.T; 
           contextPath : TEXT; 
           VAR     ctr : CARDINAL) RAISES { Wr.Failure };
  END;

  R = T BRANDED OBJECT (* resistor *)
    r : LONGREAL;
  END;

  C = T BRANDED OBJECT (* capacitor *)
    c : LONGREAL;
  END;

  X = T OBJECT         (* subcell *)
    type : TEXT;
  END;

  M = T OBJECT         (* transistor *)
  END;

CONST Brand = "SpiceObject";

CONST Equal = Refany.Equal;

PROCEDURE Hash(a : T) : Word.T;
  
END SpiceObject.
