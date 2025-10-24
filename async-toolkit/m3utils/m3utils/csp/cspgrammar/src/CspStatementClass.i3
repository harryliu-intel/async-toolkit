INTERFACE CspStatementClass;

TYPE
  Assignment = T OBJECT
    lhs, rhs : CspExpression.T;
  END;
  
END CspStatementClass.
