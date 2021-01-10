INTERFACE Polynomial;
IMPORT Mpfr;
IMPORT SortedLongrealMpfrTbl;

TYPE
  T <: REFANY;

  (* careful with the mixed types here

     coefficients are all Mpfr.T

     powers are LONGREAL 

     we can take integral (CARDINAL) powers of the expressions
  *)

PROCEDURE MakeConstant(x : Mpfr.T) : T;

PROCEDURE MakePower(x : LONGREAL) : T;

PROCEDURE Plus(a, b : T) : T;

PROCEDURE Times(a, b : T) : T;

PROCEDURE IntPow(a : T; k : CARDINAL) : T;

PROCEDURE Iterate(a : T) : Iterator;

PROCEDURE SetPrec(to : CARDINAL);

PROCEDURE GetPrec() : CARDINAL;

PROCEDURE DebugFmt(a : T) : TEXT;
  
CONST DefPrec = 200;
      
TYPE
  Iterator = SortedLongrealMpfrTbl.Iterator;

CONST Brand = "Polynomial";

END Polynomial.
