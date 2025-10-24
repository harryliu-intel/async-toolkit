GENERIC INTERFACE MultiEval(Field, Type, TypeSeq);
IMPORT LRVector;
IMPORT Word;
IMPORT Pathname;
IMPORT GenOpt;

TYPE
  T <: Public;

  Public = Field.T OBJECT
    base : Field.T;
  METHODS
    init(base : Field.T) : T;
    multiEval(at : LRVector.T; samples : CARDINAL) : Result RAISES { GenOpt.OutOfDomain } ;
    nominalEval(at : LRVector.T) : Type.T RAISES { GenOpt.OutOfDomain } ;

    inDomain(at : LRVector.T) : BOOLEAN;
    (* check if point is in domain *)
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
