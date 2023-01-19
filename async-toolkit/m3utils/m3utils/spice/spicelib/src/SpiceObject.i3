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

  RealValue = OBJECT END;

  RealLiteral = RealValue OBJECT v : LONGREAL END;

  RealExpression = RealValue OBJECT x : TEXT END;

  R = T BRANDED Brand & " Resistor" OBJECT    (* resistor *)
    r : RealValue;
  END;

  C = T BRANDED Brand & " Capacitor" OBJECT   (* capacitor *)
    c : RealValue;
  END;

  L = T BRANDED Brand & " Inductor" OBJECT    (* inductor *)
    l : RealValue;
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

PROCEDURE FmtReal(rv : RealValue) : TEXT;
  
END SpiceObject.
