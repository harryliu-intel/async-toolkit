INTERFACE MELRType;
IMPORT Math;

TYPE T = LONGREAL;

CONST Brand = "MELRType";

CONST Null = FIRST(LONGREAL);

PROCEDURE Format(READONLY a : T) : TEXT;

PROCEDURE Plus(READONLY a, b : T) : T;

PROCEDURE Minus(READONLY a, b : T) : T;

PROCEDURE ScalarMul(READONLY a, b : T) : T;

CONST Times = ScalarMul;

PROCEDURE Abs(READONLY a : T) : T;

CONST Sqrt = Math.sqrt;
      
END MELRType.
  
