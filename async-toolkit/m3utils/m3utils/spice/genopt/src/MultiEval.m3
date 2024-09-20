MODULE MultiEval;

IMPORT MultiEvalClass AS Class;

IMPORT LRVector;
IMPORT LRScalarField;

REVEAL
  T = Class.Private BRANDED Brand OBJECT
  OVERRIDES
    eval     := Eval;
    evalHint := EvalHint;
    init     := Init;
  END;

PROCEDURE Init(t : T; base : LRScalarField.T) : T =
  BEGIN
    t.base := base;
    RETURN t
  END Init;
  
PROCEDURE Eval(t : T; p : LRVector.T) : LONGREAL =
  BEGIN
    RETURN t.base.eval(p)
  END Eval;

PROCEDURE EvalHint(t : T; p : LRVector.T) =
  BEGIN
    t.base.evalHint(p)
  END EvalHint;

BEGIN END MultiEval.
