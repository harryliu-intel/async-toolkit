(* $Id$ *)

GENERIC INTERFACE Factory(Of);
IMPORT Word;
(* A Factory.T is an object that allocates objects of the type Of.T *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    build() : Of.T
  END;

CONST Brand = "Factory of " & Of.Brand;

PROCEDURE Hash(a : T) : Word.T; 

(* Equal returns TRUE ... *)
PROCEDURE Equal(a, b : T) : BOOLEAN;


END Factory.
