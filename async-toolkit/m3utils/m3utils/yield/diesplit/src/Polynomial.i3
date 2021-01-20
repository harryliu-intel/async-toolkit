INTERFACE Polynomial;

(* 
   Simple polynomials, for yield calculations

   The implementation internally uses SortedLongrealMpfrTbl, and while
   this implementation is not exposed to clients, we borrow the implementation
   of the iterator from the more generic code.

   Author: Mika Nystrom <mika.nystroem@intel.com>
   January, 2021
*)

IMPORT Mpfr;
IMPORT SortedLongrealMpfrTbl;

TYPE
  T <: REFANY;

  (* careful with the mixed types here

     coefficients are all Mpfr.T

     powers are LONGREAL 

     we can take integral (CARDINAL) powers of the expressions
  *)

TYPE
  Coefficient = Mpfr.T;
  Exponent    = LONGREAL;

PROCEDURE MakeConstant(c : Coefficient) : T;
  (* return the polynomial c x^0 *)  

PROCEDURE MakePower(y : Exponent) : T;
  (* return the polynomial 1 x^y *)

PROCEDURE Plus(a, b : T) : T;
  (* return the polynomial a + b *)

PROCEDURE Times(a, b : T) : T;
  (* return the polynomial a * b *)

EXCEPTION Remainder;
EXCEPTION DivisionByZero;

TYPE DivResult = RECORD quotient, remainder : T END;
     
PROCEDURE LongDivide(n, d : T; remainderOk : BOOLEAN) : DivResult
  RAISES { Remainder, DivisionByZero };
  (* polynomial division
     RAISES Remainder iff 
     remainderOk = FALSE AND NOT Mpfr.ZeroP(result.remainder) 
   *)

PROCEDURE IntPow(a : T; k : CARDINAL) : T;
  (* return the polynomial a ^ k *)

PROCEDURE ScaleExponents(a : T; by : Exponent) : T;
  (* return the polynomial with all exponents scaled by by *)

PROCEDURE ZeroP(a : T) : BOOLEAN;
  (* return TRUE iff a is the zero polynomial *)

TYPE
  Iterator = SortedLongrealMpfrTbl.Iterator;

PROCEDURE Iterate(a : T) : Iterator;
  (* iterate through the monomial terms of the polynomial

     If iter.next(y, c) returns TRUE, then the polynomial contains a term
     c x^y 
  *)

PROCEDURE SetPrec(to : CARDINAL);
  (* set the underlying Mpfr precision of numbers used by this module *)

PROCEDURE GetPrec() : CARDINAL;

CONST DefPrec = 200; (* default precision *)

PROCEDURE DebugFmt(a : T) : TEXT;
  (* format a polynomial for debugging *)
  
CONST Brand = "Polynomial";

END Polynomial.
