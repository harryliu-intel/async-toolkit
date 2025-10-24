INTERFACE LibertyAttrValExpr;
IMPORT LibertyComponent;
IMPORT LibertyBoolean;
IMPORT LibertyExpr;

TYPE
  T <: LibertyComponent.T;

  String = T OBJECT
    val : TEXT;
  END;

  Boolean = T OBJECT
    val : LibertyBoolean.T;
  END;

  Expr = T OBJECT
    val : LibertyExpr.T;
  END;

CONST Brand = "LibertyAttrValExpr";

END LibertyAttrValExpr.
