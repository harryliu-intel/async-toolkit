(* $Id$ *)

INTERFACE BoolInteger;
IMPORT Bool, BoolSet;
IMPORT Word;

(* A BoolInteger.T represents the non-empty set of values that an
   integer expression can take on. *)

TYPE
  T <: Private;

  Private <: Public;

  Public = OBJECT METHODS

    (* initialize a new T as a range with min value and max value *)
    init(max : INTEGER; min : INTEGER := 0) : T;

    getMinValue() : INTEGER;
    getMaxValue() : INTEGER;
    isConstant() : BOOLEAN;
    
    extract(bit : CARDINAL) : Bool.T;

    isEqual(value : INTEGER) : Bool.T;

    (* return an upper bound on how many bits it takes to represent
       the number in two's complement *)
    repbits() : CARDINAL;
  END;

CONST Brand = "BoolInteger";

(* Equal(x,y) is equivalent to Equals(x,y) = Bool.True() *)
(* because the objects are cached, this is also equivalent to a = b *)
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
PROCEDURE Sub(a, b : T) : T;
PROCEDURE Abs(a : T) : T;
PROCEDURE Mul(a, b : T) : T;

(* Sign returns true for a negative number, false for a positive one *)
PROCEDURE Sign(a : T) : Bool.T;

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

(* positive shifts are left, negative shifts are right *)
PROCEDURE UnsignedShift(a : T; sa : INTEGER) : T;
PROCEDURE SignedShift(a : T; sa : INTEGER) : T;
PROCEDURE UnsignedShiftV(a : T; sa : T) : T;
PROCEDURE SignedShiftV(a : T; sa : T) : T;


(* choose "if" if c is false, "it" if c is true *)
PROCEDURE Choose(c : Bool.T; if, it : T) : T;

PROCEDURE Constant(c : INTEGER) : T;
PROCEDURE Vars(a : T) : BoolSet.T;

END BoolInteger.
