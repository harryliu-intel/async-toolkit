INTERFACE Complex;

(* simple complex numbers *)

TYPE
  T = RECORD
    re, im : LONGREAL;
  END;

PROCEDURE Real(READONLY t : T) : LONGREAL;

PROCEDURE Imag(READONLY t : T) : LONGREAL;

PROCEDURE Construct(re, im : LONGREAL := 0.0d0) : T;

PROCEDURE Polar(arg, mag : LONGREAL) : T;

(**********************************************************************)

PROCEDURE Conj(READONLY t : T) : T;

PROCEDURE Recip(READONLY t : T) : T;

PROCEDURE Arg(READONLY t : T) : LONGREAL; 

PROCEDURE Mag(READONLY t : T) : LONGREAL;

PROCEDURE MagSq(READONLY t : T) : LONGREAL;

PROCEDURE Cis(x : LONGREAL) : T;

PROCEDURE Log(READONLY t : T) : T;

PROCEDURE Exp(READONLY t : T) : T;

(**********************************************************************)

PROCEDURE Plus(READONLY a, b : T) : T;

PROCEDURE Minus(READONLY a, b : T) : T;

PROCEDURE Times(READONLY a, b : T) : T;

PROCEDURE Divide(READONLY a, b : T) : T;

PROCEDURE Pow(READONLY a, b : T) : T;
  
CONST Brand = "Complex";
      
END Complex.
