(* $Id$ *)
INTERFACE BigInt;
IMPORT Word;

CONST Brand = "BigInt";

TYPE T        <: Public;
TYPE Natural  = T;

TYPE Public = ROOT;

TYPE CompRet = [-1..1];

PROCEDURE Compare(a, b : T) : CompRet;
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE New(x : INTEGER) : T;
PROCEDURE Div(a, b : T) : T;
PROCEDURE Mul(a, b : T) : T;
PROCEDURE Add(a, b : T) : T;
PROCEDURE Abs(a : T) : T;
PROCEDURE Mod(a, b : T) : T;
PROCEDURE Sign(a : T) : CompRet;

PROCEDURE Format(a : T; base : CARDINAL := 10) : TEXT;

VAR (* CONST *) Zero, One : T;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE ToLongReal(a : T) : LONGREAL;
PROCEDURE Max(a, b : T) : T;
PROCEDURE Min(a, b : T) : T;

END BigInt.
