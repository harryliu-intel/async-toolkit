INTERFACE P3;
IMPORT Random;

TYPE
  T = ARRAY [0..2] OF LONGREAL;

CONST Brand = "P3";

PROCEDURE Format(READONLY p : T; prec : CARDINAL := 3) : TEXT;

PROCEDURE FormatGnu(READONLY p : T) : TEXT;

PROCEDURE Minus(READONLY a, b : T) : T;

PROCEDURE Dot(READONLY a, b : T) : LONGREAL;

PROCEDURE Cross(READONLY a, b : T) : T;

PROCEDURE Norm(READONLY a : T) : LONGREAL;

PROCEDURE RandomDirection(rand : Random.T := NIL) : T;
  (* pick a random direction using the Archimedes cylinder construction *)
  
END P3.
