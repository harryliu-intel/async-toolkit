(* $Id$ *)

INTERFACE BoolInteger;
IMPORT Bool, BoolSet;
IMPORT Word;

(* A BoolInteger.T represents an integer expression that can have a
   value over a range from some minimum value minValue to some maximum
   value maxValue *)

TYPE
  T <: Public;

  Public = OBJECT METHODS

    (* initialize a new T with min value 0 and max value maxValue *)
    init(maxValue : CARDINAL) : T;

    getMinValue() : INTEGER;
    getMaxValue() : INTEGER;
    isConstant() : BOOLEAN;
    
    extract(bit : CARDINAL) : Bool.T;

    isEqual(value : INTEGER) : Bool.T;
  END;

CONST Brand = "BoolInteger";

(* This is a different Equal, only tests pointer equivalence; for generics *)
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

(* some useful constants... *)
(* CONST *)VAR One, Zero, MinusOne : T; 

(* comparisons *)
PROCEDURE Equals(a, b : T) : Bool.T;
PROCEDURE LessThanZero(a : T) : Bool.T;
PROCEDURE GreaterThan(a, b : T) : Bool.T;
PROCEDURE LessThan(a, b : T) : Bool.T;
PROCEDURE GreaterThanOrEqual(a, b : T) : Bool.T;
PROCEDURE LessThanOrEqual(a, b : T) : Bool.T;

(* arithmetic *)
PROCEDURE Add(a, b : T) : T;
PROCEDURE Neg(a : T) : T;
PROCEDURE Minus(a, b : T) : T;

(* bitwise ops *)
PROCEDURE BitwiseAnd(a, b : T) : T;
PROCEDURE BitwiseOr(a, b : T) : T;
PROCEDURE BitwiseXor(a, b : T) : T;
PROCEDURE BitwiseNot(a : T) : T;

(* general bitwise op *)
PROCEDURE BitwiseOp(a, b : T; op : PROCEDURE(a, b : Bool.T) : Bool.T) : T;

(* shifts by a constant amount (in base 2) *)
PROCEDURE ShiftLeft(a : T; sa : CARDINAL) : T;
PROCEDURE UnsignedShiftRight(a : T; sa : CARDINAL) : T;
PROCEDURE SignedShiftRight(a : T; sa : CARDINAL) : T ;

PROCEDURE Constant(c : INTEGER) : T;
PROCEDURE Vars(a : T) : BoolSet.T;

END BoolInteger.
