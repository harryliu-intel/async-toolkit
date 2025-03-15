INTERFACE CspExpressionClass;
IMPORT CspExpression;

TYPE
  Binary = T OBJECT
    op : BinaryOp;
    l, r : T;
  END;

  Unary = T OBJECT
    op : UnaryOp;
    x  : T;
  END;

  MemberAccess = T OBJECT
  END;

  Probe = T OBJECT
    channel : T;
  END;

  Receive = T OBJECT
    channel : T;
  END;

END CspExpressionClass.
