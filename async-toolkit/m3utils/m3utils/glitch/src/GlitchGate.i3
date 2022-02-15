INTERFACE GlitchGate;
IMPORT GlitchExpr;
IMPORT BDD;

TYPE
  T = OBJECT
    tgt    : TEXT;
    tgtBdd : BDD.T;
    expr   : GlitchExpr.T;
  END;

PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "GlitchGate";

END GlitchGate.
