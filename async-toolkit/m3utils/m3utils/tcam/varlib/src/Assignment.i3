INTERFACE Assignment;
IMPORT VarExpr;

TYPE
  T = RECORD
    tgt : TEXT;
    val : VarExpr.T;
    pct := FALSE;
  END;

CONST Brand = "Assignment";

CONST Equal : PROCEDURE(READONLY a, b : T) : BOOLEAN = NIL;

END Assignment.
  
