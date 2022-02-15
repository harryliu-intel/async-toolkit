INTERFACE GlitchExpr;
IMPORT BDD;
IMPORT Text01XTbl;
IMPORT ZeroOneX;
IMPORT TextSet;

TYPE
  T = OBJECT nm : TEXT; x : BDD.T END;

  Named = T BRANDED OBJECT
  END;

  Expr = T BRANDED OBJECT
    op : Op;
    a, b : T;
  END;

  Op = { And, Or, Not };

PROCEDURE And(a, b : T) : T;
PROCEDURE Or(a, b : T) : T;
PROCEDURE Not(a : T) : T;
PROCEDURE New(nm : TEXT) : T;

CONST Brand = "GlitchExpr";

PROCEDURE Fanins(t : T) : TextSet.T;
  
END GlitchExpr.
