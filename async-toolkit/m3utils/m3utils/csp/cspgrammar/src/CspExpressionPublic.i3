INTERFACE CspExpressionPublic;
IMPORT CspExpression;
IMPORT CspExpressionSeq;
IMPORT Atom;
IMPORT CspRange;

REVEAL
  CspExpression.FunctionCall <: PublicFunctionCall;
  CspExpression.Loop         <: PublicLoop;
  
TYPE
  PublicFunctionCall = CspExpression.T OBJECT
    f    : CspExpression.T;
    args : CspExpressionSeq.T;
  END;

  PublicLoop = CspExpression.T OBJECT
    dummy : Atom.T;
    range : CspRange.T;
    op    : CspExpression.BinaryOp;
    x     : CspExpression.T;
  END;
  
END CspExpressionPublic.
