INTERFACE GlitchExprEval;
IMPORT TextGlitchExprTbl;
IMPORT GlitchExpr;
IMPORT Text01XTbl;
IMPORT ZeroOneX;

PROCEDURE Eval(x        : GlitchExpr.T;
               literals : Text01XTbl.T;
               gates    : TextGlitchExprTbl.T) : ZeroOneX.T;
  
END GlitchExprEval.
