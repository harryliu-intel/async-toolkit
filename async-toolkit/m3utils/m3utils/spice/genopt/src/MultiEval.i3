INTERFACE MultiEval;
IMPORT LRVector;
IMPORT LRScalarField;
IMPORT Word;

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
    id         : Word.T;
    nominal    : LONGREAL := 0.0d0;
    n          : CARDINAL;
    sum, sumsq : LONGREAL;
  END;

CONST Brand = "MultiEval";

PROCEDURE Nominal(READONLY a : Result) : LONGREAL;

PROCEDURE Mean(READONLY a : Result) : LONGREAL;

PROCEDURE Var(READONLY a : Result) : LONGREAL;
  (* uncorrected variance *)

PROCEDURE Sdev(READONLY a : Result) : LONGREAL;
  (* sample-unbiased standard deviation *)

PROCEDURE Combine(READONLY a, b : Result) : Result;

PROCEDURE Format(READONLY a : Result) : TEXT;
  
END MultiEval.
