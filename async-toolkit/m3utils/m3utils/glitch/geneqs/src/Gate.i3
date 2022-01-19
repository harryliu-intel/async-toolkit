INTERFACE Gate;
IMPORT GateExpr;

TYPE
  T = RECORD
    tgt  : TEXT;
    expr : GateExpr.T;
  END;

PROCEDURE New(named : TEXT; expr : GateExpr.T) : T;

PROCEDURE Last() : T;
  
PROCEDURE GetLiteral(named : TEXT) : GateExpr.T;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

CONST Brand = "Gate";

PROCEDURE Format(READONLY a : T) : TEXT;
  
END Gate.
