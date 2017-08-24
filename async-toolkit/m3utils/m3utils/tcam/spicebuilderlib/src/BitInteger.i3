INTERFACE BitInteger;
IMPORT Bit;

TYPE
  T    = BRANDED OBJECT END;

  SmallPromise <: PubSmallPromise;

  PubSmallPromise = T BRANDED OBJECT 
    v : INTEGER ;
  METHODS
    force(bits : CARDINAL) : Concrete;
  END;

  Concrete = T BRANDED OBJECT bits : REF ARRAY OF Bit.T END;

CONST Brand = "Integer";

PROCEDURE Small(z : INTEGER) : T;

END BitInteger.
