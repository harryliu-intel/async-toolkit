(* $Id$ *)

INTERFACE BDDDepender;
IMPORT BDD, BDDSet;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;
    depends(b : BDD.T) : BDDSet.T;
    isPlainOr(b : BDD.T) : BOOLEAN;
  END;

CONST Brand = "BDDDepender";

END BDDDepender.
