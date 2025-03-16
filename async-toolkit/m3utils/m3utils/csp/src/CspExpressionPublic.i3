INTERFACE CspExpressionPublic;
IMPORT CspExpression;
IMPORT CspExpressionSeq;

REVEAL
  CspExpression.FunctionCall <: PublicFunctionCall;

TYPE
  PublicFunctionCall = CspExpression.T OBJECT
    f    : CspExpression.T;
    args : CspExpressionSeq.T;
  END;

END CspExpressionPublic.
