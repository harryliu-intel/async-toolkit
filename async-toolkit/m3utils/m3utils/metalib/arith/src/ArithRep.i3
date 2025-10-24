INTERFACE ArithRep;
IMPORT ArithR, ArithP;
IMPORT Arith, SchemeObject;
IMPORT Atom;

TYPE
  XY = { X, Y };

  R = ArithR.T;
  P = ArithP.T;
  F = ArithR.F;

  RLiteral  = R BRANDED OBJECT name   : Atom.T    END;

  RConstant = R BRANDED OBJECT value  : LONGREAL  END;

  RRange    = R BRANDED OBJECT x1, x2 : LONGREAL  END;

  RPlus     = R BRANDED OBJECT a, b   : R         END;

  RFromPair = R BRANDED OBJECT p : P; whch : XY   END;
  
  PPair     = P BRANDED OBJECT x, y   : R         END;

  RSelectP  = P BRANDED OBJECT 
                                    by     : REF ARRAY OF R;
                                    val    : REF ARRAY OF P;
  END;

  RFunc     = R BRANDED OBJECT f : F; of : R      METHODS n() : TEXT END;
    
PROCEDURE ToScheme(x : Arith.T) : SchemeObject.T;

PROCEDURE ToSchemePlain(x : Arith.T) : SchemeObject.T;

END ArithRep.
