(* $Id: IntSetBits.i3,v 1.1 2009/03/27 19:14:54 mika Exp $ *)

INTERFACE IntSetBits;
IMPORT IntSet;

TYPE
  T <: Public;

  Public = IntSet.T OBJECT METHODS
    init(minValHint := 0; maxValHint := 1) : T;
  END;

CONST Brand = "IntSetBits";

END IntSetBits.
    
