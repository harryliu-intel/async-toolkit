(* $Id$ *)
INTERFACE ObjectFactory;
IMPORT Word;

(* A Factory.T is an object that allocates objects *)

TYPE
  T = OBJECT 
    code : Word.T;
  METHODS
    init() : T;
    build() : REFANY;
    hash() : Word.T;
    equal(a : T) : BOOLEAN;
  END;

PROCEDURE Hash(a : T) : Word.T; (* call hash method *)
PROCEDURE Equal(a, b : T) : BOOLEAN; (* call a.equal(b) *)

END ObjectFactory.
    
