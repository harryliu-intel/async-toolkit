(* $Id: FactorialValues.i3,v 1.5 2009/11/12 18:56:55 mika Exp $ *)

INTERFACE FactorialValues;

EXCEPTION IndexOutOfBounds;

TYPE
  T <: Public;

  Public = OBJECT 
    named : TEXT; 
    n : CARDINAL;
  METHODS
    formatV(idx : CARDINAL) : TEXT RAISES { IndexOutOfBounds };
    toLongreal(idx : CARDINAL) : LONGREAL RAISES { IndexOutOfBounds };
    (* convert an index to a longreal according to some method...
       matches numbers for numbers, returns index for others (including bool)
    *)
  END;

  Int <: PubInt;
  PubInt = T OBJECT
    v : REF ARRAY OF INTEGER
  END;

  Bool <: PubBool;
  PubBool = T OBJECT
    v : REF ARRAY OF BOOLEAN
  END;

  LR <: PubLR;
  PubLR = T OBJECT
    v : REF ARRAY OF LONGREAL
  END;

  TX <: PubTX;
  PubTX = T OBJECT
    v : REF ARRAY OF TEXT
  END;

  Type = { Int, Bool, LR, Text };

PROCEDURE TypeOf(t : T) : Type;

CONST Brand = "FactorialValues";

END FactorialValues.


