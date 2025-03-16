MODULE CspExpression;
IMPORT CspExpressionPublic AS P;

REVEAL
  Boolean = PublicBoolean BRANDED Brand & " Boolean" OBJECT
  END;
  
  Integer = PublicInteger BRANDED Brand & " Integer" OBJECT
  END;
  
  String = PublicString BRANDED Brand & " String" OBJECT
  END;
  
  (**********************************************************************)

  Identifier = PublicIdentifier BRANDED Brand & " Identifier" OBJECT
  END;

  (**********************************************************************)
  
  Binary = PublicBinary BRANDED Brand & " Binary" OBJECT
  END;
  
  Unary = PublicUnary BRANDED Brand & " Unary" OBJECT
  END;

  ArrayAccess = PublicArrayAccess BRANDED Brand & " ArrayAccess" OBJECT
  END;
  
  MemberAccess = PublicAccess BRANDED Brand & " MemberAccess" OBJECT
  END;
  
  StructureAccess = PublicAccess BRANDED Brand & " StructureAccess" OBJECT
  END;

  BitRange = PublicBitRange BRANDED Brand & " BitRange" OBJECT
  END;
  
  Probe = ChanExpr BRANDED Brand & " Probe" OBJECT
  END;
  
  Receive = ChanExpr BRANDED Brand & " Receive" OBJECT
  END;

  Peek = ChanExpr BRANDED Brand & " Peek" OBJECT
  END;
  

  FunctionCall = P.PublicFunctionCall BRANDED Brand & " FunctionCall" OBJECT
  END;

BEGIN END CspExpression.
