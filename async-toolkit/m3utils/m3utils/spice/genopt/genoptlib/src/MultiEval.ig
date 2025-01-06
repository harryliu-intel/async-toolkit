GENERIC INTERFACE MultiEval(Field, Type, TypeSeq);
IMPORT LRVector;
IMPORT Word;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = Field.T OBJECT
    base : Field.T;
  METHODS
    init(base : Field.T) : T;
    multiEval(at : LRVector.T; samples : CARDINAL) : Result;
    nominalEval(at : LRVector.T) : Type.T;
  END;

TYPE
  Result = RECORD
    id         : Word.T;
    nominal    : Type.T := Type.Null;
    n          : CARDINAL;
    sum, sumsq : Type.T;
    extra      : REFANY;
    subdirPath : Pathname.T;
    seq        : TypeSeq.T;
  END;

CONST Brand = "MultiEval(" & Field.Brand & "," & Type.Brand & ")";

PROCEDURE Nominal(READONLY a : Result) : Type.T;

PROCEDURE Mean(READONLY a : Result) : Type.T;

PROCEDURE Var(READONLY a : Result) : Type.T;
  (* uncorrected variance *)

PROCEDURE Sdev(READONLY a : Result) : Type.T;
  (* sample-unbiased standard deviation *)

PROCEDURE Combine(READONLY a, b : Result) : Result;

PROCEDURE Format(READONLY a : Result) : TEXT;
  
END MultiEval.
