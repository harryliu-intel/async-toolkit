INTERFACE MultiEval;
IMPORT LRVector;
IMPORT LRScalarField;

TYPE
  T <: Public;

  Public = LRScalarField.T OBJECT
  METHODS
    init(base : LRScalarField.T) : T;
    multiEval(at : LRVector.T; samples : CARDINAL) : Result;
  END;

TYPE
  Result = RECORD
    n         : CARDINAL;
    mean, var : LONGREAL;
  END;

CONST Brand = "MultiEval";

END MultiEval.
