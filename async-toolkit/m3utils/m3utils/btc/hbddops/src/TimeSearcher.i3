(* $Id$ *)

INTERFACE TimeSearcher;
IMPORT BDDCausal, BDDBDDTbl;

TYPE
  T <: PublicTS;

  PublicTS = BDDCausal.T OBJECT METHODS
    init(eqns : BDDBDDTbl.T) : T;
  END;

CONST Brand = "TimeSearcher";

END TimeSearcher.
