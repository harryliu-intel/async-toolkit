INTERFACE CardTriple;

TYPE T = ARRAY [0..2] OF CARDINAL;

CONST Brand = "CardTriple";

PROCEDURE Add(READONLY a, b : T) : T;

END CardTriple.
