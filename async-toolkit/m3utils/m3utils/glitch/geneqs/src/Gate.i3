INTERFACE Gate;
IMPORT GateExpr;
IMPORT TextSet;

TYPE
  T = RECORD
    tgt    : TEXT;
    expr   : GateExpr.T;
    fanins : TextSet.T; (* not filled in by parser *)
  END;

PROCEDURE New(named : TEXT; expr : GateExpr.T) : T;

PROCEDURE Last() : T;
  
PROCEDURE GetLiteral(named : TEXT) : GateExpr.T;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

CONST Brand = "Gate";

PROCEDURE Format(READONLY a : T; ass := " <- "; not := "!" ) : TEXT;

END Gate.
