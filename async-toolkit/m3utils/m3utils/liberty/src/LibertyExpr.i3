INTERFACE LibertyExpr;
IMPORT LibertyComponent;
IMPORT LibertyNumber;

TYPE
  T <: LibertyComponent.T;

  Op = { Plus, Minus, Times, Div, Uminus, Uplus };
  
  Binary = T OBJECT
    op : Op;
    a, b : T;
  END;

  Unary = T OBJECT
    op : Op;
    a : T;
  END;

  Type = { Ident, Num };

  Const = T OBJECT
    type : Type;
    val  : REFANY;
  END;

PROCEDURE Plus(a, b : T) : T;
PROCEDURE Minus(a, b : T) : T;
PROCEDURE Times(a, b : T) : T;
PROCEDURE Div(a, b : T) : T;
PROCEDURE Uminus(a : T) : T;
PROCEDURE Uplus(a : T) : T;
PROCEDURE Num(n : LibertyNumber.T) : T;
PROCEDURE Ident(n : TEXT) : T;

CONST Brand = "LibertyExpr";

END LibertyExpr.
