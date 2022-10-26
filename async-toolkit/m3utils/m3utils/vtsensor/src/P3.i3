INTERFACE P3;
IMPORT Random;
IMPORT Word;

TYPE
  T = ARRAY [0..2] OF LONGREAL;

CONST Brand = "P3";

PROCEDURE Format(READONLY p : T; prec : CARDINAL := 3) : TEXT;

PROCEDURE FormatGnu(READONLY p : T) : TEXT;

PROCEDURE Minus(READONLY a, b : T) : T;

PROCEDURE Plus(READONLY a, b : T) : T;

PROCEDURE Dot(READONLY a, b : T) : LONGREAL;

PROCEDURE Cross(READONLY a, b : T) : T;

PROCEDURE Norm(READONLY a : T) : LONGREAL;

PROCEDURE ScalarMul(m : LONGREAL; a : T) : T;

PROCEDURE RandomDirection(rand : Random.T := NIL) : T;
  (* pick a random direction using the Archimedes cylinder construction *)

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

CONST Zero = T { 0.0d0, .. };
  
END P3.
