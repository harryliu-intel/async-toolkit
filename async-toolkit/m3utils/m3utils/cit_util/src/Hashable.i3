(* $Id: Hashable.i3,v 1.1 2011/01/31 02:31:59 mika Exp $ *)


INTERFACE Hashable;
IMPORT Word;

TYPE
  T = OBJECT METHODS
    hash() : Word.T;
    equal(ref : REFANY) : BOOLEAN;
  END;

CONST Brand = "Hashable";

PROCEDURE Hash(a : T) : Word.T;
  (* a.hash() *)

PROCEDURE Equal(a, b : T) : BOOLEAN;
  (* a.equal(b) *)

END Hashable.


  
