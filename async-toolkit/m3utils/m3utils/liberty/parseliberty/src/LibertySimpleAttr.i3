INTERFACE LibertySimpleAttr;
IMPORT LibertyComponent;
IMPORT LibertyAttrValExpr;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    ident       : TEXT;
    attrValExpr : LibertyAttrValExpr.T;
    syntax      : Syntax;
  END;

  Syntax = { ColonSemi, Colon, Eq };
  
CONST Brand = "LibertySimpleAttr";

END LibertySimpleAttr.
 
