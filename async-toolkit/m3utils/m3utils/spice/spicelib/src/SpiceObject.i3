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

  R = T BRANDED Brand & " Resistor" OBJECT    (* resistor *)
    r : LONGREAL;
  END;

  C = T BRANDED Brand & " Capacitor" OBJECT   (* capacitor *)
    c : LONGREAL;
  END;

  L = T BRANDED Brand & " Inductor" OBJECT    (* inductor *)
    l : LONGREAL;
  END;

  X = T BRANDED Brand & " Subcell" OBJECT     (* subcell *)
    type : TEXT;
  END;

  M = T BRANDED Brand & " MOSFET" OBJECT      (* transistor *)
    type : TEXT;
  END;

  D = T BRANDED Brand & " Diode" OBJECT       (* diode *)
    type : TEXT;
  END;

CONST Brand = "SpiceObject";

CONST Equal = Refany.Equal;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE Format(a : T) : TEXT;
  
END SpiceObject.
