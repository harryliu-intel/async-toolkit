INTERFACE GateExpr;

TYPE
  T = OBJECT nm : TEXT END;

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

CONST Brand = "GateExpr";

PROCEDURE Format(a : T) : TEXT;
  
END GateExpr.
