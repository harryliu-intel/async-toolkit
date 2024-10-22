INTERFACE MELRVectorType;
IMPORT LRVector;

TYPE T = LRVector.T;

CONST Brand = "MELRType";

CONST Null : T = NIL;

PROCEDURE Format(READONLY a : T) : TEXT;

PROCEDURE Plus(READONLY a, b : T) : T;

PROCEDURE Minus(READONLY a, b : T) : T;

PROCEDURE ScalarMul(a : LONGREAL; READONLY b : T) : T;

PROCEDURE Times(READONLY a, b : T) : T;

PROCEDURE Abs(READONLY a : T) : T;

PROCEDURE Sqrt(READONLY a : T) : T;

END MELRVectorType.
  
