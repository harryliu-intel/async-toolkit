(* $Id: ObjectFactory.i3,v 1.3 2008/05/01 03:37:14 mika Exp $ *)
INTERFACE ObjectFactory;
IMPORT Word;

(* A Factory.T is an object that allocates objects *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : Public;
    build() : REFANY;
    hash() : Word.T;
    equal(a : T) : BOOLEAN;
  END;

  (* the default implementation allows you to make a new factory with
     a given typecode *)
  Default <: PublicDefault;

  PublicDefault = T OBJECT METHODS
    init(typecode : Word.T) : T;
  END;

PROCEDURE Hash(a : T) : Word.T; (* call hash method *)
PROCEDURE Equal(a, b : T) : BOOLEAN; (* call a.equal(b) *)

CONST Brand = "ObjectFactory";

END ObjectFactory.
    
