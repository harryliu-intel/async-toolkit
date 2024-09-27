INTERFACE MultiEval;
IMPORT LRVector;
IMPORT LRScalarField;

TYPE
  T <: Public;

  Public = LRScalarField.T OBJECT
  METHODS
    init(base : LRScalarField.T) : T;
    multiEval(at : LRVector.T; samples : CARDINAL) : Result;
    nominalEval(at : LRVector.T) : LONGREAL;
  END;

TYPE
  Result = RECORD
    n          : CARDINAL;
    sum, sumsq : LONGREAL;
  END;

CONST Brand = "MultiEval";

PROCEDURE Mean(READONLY a : Result) : LONGREAL;

PROCEDURE Var(READONLY a : Result) : LONGREAL;
  (* uncorrected variance *)

PROCEDURE Sdev(READONLY a : Result) : LONGREAL;
  (* sample-unbiased standard deviation *)

PROCEDURE Combine(READONLY a, b : Result) : Result;

PROCEDURE Format(READONLY a : Result) : TEXT;
  
END MultiEval.
