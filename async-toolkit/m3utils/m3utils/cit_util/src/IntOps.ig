GENERIC INTERFACE IntOps(Int, TextIntTbl);
IMPORT Random;
IMPORT Pathname;
(* $Id$ *)

TYPE
  T = Int.T;


PROCEDURE Scan(t: TEXT; base : CARDINAL := 10) : T;
(* base <= 16 *)

PROCEDURE Exp(base, exp: T): T;
(* exp must be nonnegative *)

PROCEDURE ModExp(base, exp, mod: T): T;
(* mod must be positive; if exp is negative, base must have inverse *)

PROCEDURE Rand(source: Random.T; lessThan: T): T;
(* lessThan must be positive *)

PROCEDURE ProbablyPrime(p: T): BOOLEAN;
(* returns FALSE for most composites; always TRUE for primes *)

PROCEDURE RelPrime(a,b: T): BOOLEAN;


(* complete basic arithmetic *)

CONST
  New = Int.New;
  Add = Int.Add;
  Mul = Int.Mul;
  Mod = Int.Mod;
  Div = Int.Div;
  Sign = Int.Sign;
  Compare = Int.Compare;
  Equal = Int.Equal;
  Divide = Int.Divide;
PROCEDURE Negate(a: T): T;
PROCEDURE Sub(a, b: T): T;
PROCEDURE Old(a: T): INTEGER; (* convert small ints back to INTEGER *)


(* misc *)

EXCEPTION
  NoneExists;
CONST
  Brand = "Ops(" & Int.Brand & ")";
PROCEDURE GCD(a, b: T): T;
PROCEDURE ExtendedGCD(a, b: T; VAR aCoeff, bCoeff: T): T;
PROCEDURE ModInverse(a, mod: T): T RAISES {NoneExists};
PROCEDURE Odd(a: T): BOOLEAN;
PROCEDURE Square(a: T): T; (* x^2 *)
PROCEDURE Pred(x: T): T;   (* x-1 *)
PROCEDURE Half(x: T): T;   (* x/2 *)
PROCEDURE IsOne(x: T): BOOLEAN;
PROCEDURE One(): T;
PROCEDURE Zero(): T;


(* file I/O *)

PROCEDURE Read(fn: Pathname.T): TextIntTbl.T;
PROCEDURE Write(fn: Pathname.T; tbl: TextIntTbl.T);

END IntOps.
