INTERFACE GlitchExpr;
IMPORT BDD;
IMPORT Text01XTbl;
IMPORT ZeroOneX;

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

PROCEDURE Eval(x : T; tab : Text01XTbl.T) : ZeroOneX.T;
  
CONST Brand = "GlitchExpr";
      
END GlitchExpr.
