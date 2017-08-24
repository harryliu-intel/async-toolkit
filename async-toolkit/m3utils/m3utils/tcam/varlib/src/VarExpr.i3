INTERFACE VarExpr;
IMPORT TextSet;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    eval(getter : Getter) : LONGREAL;
    getDeps(into : TextSet.T; excluding : TextSet.T := NIL);
  END;

  Getter = OBJECT METHODS
    v(nm : TEXT) : LONGREAL;
  END;

  Func = T OBJECT
    nm : TEXT;
    args : REF ARRAY OF T;
  END;

  Variable = T OBJECT
    nm : TEXT;
  END;

  Literal = T OBJECT
    val : LONGREAL;
  END;

  UnOp = T OBJECT
    a : T;
  END;

  BinOp = T OBJECT
    a, b : T;
  END;

  TernOp = T OBJECT
    a, b, c : T;
  END;

  (**********************************************************************)

  UMinus = UnOp BRANDED OBJECT END;

  Plus = BinOp BRANDED OBJECT END;
  Minus = BinOp BRANDED OBJECT END;
  Times = BinOp BRANDED OBJECT END;
  Divide = BinOp BRANDED OBJECT END;
  Equal  = BinOp BRANDED OBJECT END;

  QMark = TernOp BRANDED OBJECT END;

CONST Brand = "VarExpr";

END VarExpr.
