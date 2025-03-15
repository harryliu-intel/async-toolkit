INTERFACE CspExpression;

TYPE
  T <: Public;

  Public = ROOT OBJECT
  END;

  BinaryOp = { Add, Sub, Div, Rem, Mul,
               EQ, GE, GT, LT, LE, NE,
               And, Or, Xor,
               SHL, SHR,
               CondAnd, CondOr };

  UnaryOp = { Neg, Not };
  
CONST Brand = "CspExpression";
      
END CspExpression.
