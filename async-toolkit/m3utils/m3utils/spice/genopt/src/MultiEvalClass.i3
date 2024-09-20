INTERFACE MultiEvalClass;
IMPORT MultiEval, LRScalarField;

REVEAL
  MultiEval.T <: Private;

TYPE
  Private = MultiEval.Public OBJECT
    base : LRScalarField.T;
  END;

END MultiEvalClass.
