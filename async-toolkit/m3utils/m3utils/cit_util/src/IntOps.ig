GENERIC INTERFACE IntOps(Int);
IMPORT Random;
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


(* complete basic arithmetic *)

CONST
  Add = Int.Add;
  Mul = Int.Mul;
  Mod = Int.Mod;
  Div = Int.Div;
  Sign = Int.Sign;
  Compare = Int.Compare;
  Equal = Int.Equal;
PROCEDURE Negate(a: T): T;
PROCEDURE Sub(a, b: T): T;


(* misc *)

EXCEPTION
  NoneExists;
CONST
  Brand = "Ops(" & Int.Brand & ")";
PROCEDURE ExtendedGCD(a, b: T; VAR aCoeff, bCoeff: T): T;
PROCEDURE ModInverse(a, mod: T): T RAISES {NoneExists};
PROCEDURE Odd(a: T): BOOLEAN;
PROCEDURE Square(a: T): T;

END IntOps.
