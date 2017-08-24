INTERFACE Perturbation;
IMPORT Refany;
IMPORT TextLongRealTbl AS TextLRTbl;

TYPE
  T  = OBJECT
    dict        : TextLRTbl.T;
  METHODS
    calc() : LONGREAL;
  END;

  Default <: PubDefault;
  
  (* specifier of perturbations inherits from here and builds the perturbation
     in the calc method *)
  PubDefault = T OBJECT METHODS
    init    (model, var : TEXT) : T;
    v       (varName : TEXT) : LONGREAL; 
  END;

CONST Brand = "Perturbation";

CONST Equal = Refany.Equal;

END Perturbation.
