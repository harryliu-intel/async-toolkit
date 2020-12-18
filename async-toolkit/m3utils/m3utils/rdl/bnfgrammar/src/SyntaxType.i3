INTERFACE SyntaxType;

(* see SyntaxTypeSystem.i3 for documentation *)

IMPORT Word;

TYPE
  T <: ROOT;

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE Compare(a, b : T) : [-1..1];

CONST Brand = "SyntaxType";

PROCEDURE Format(a : T) : TEXT;
  (* format for debugging *)
  
END SyntaxType.
