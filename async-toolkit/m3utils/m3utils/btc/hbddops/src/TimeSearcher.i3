(* $Id: TimeSearcher.i3,v 1.1 2014/02/09 11:16:08 mika Exp $ *)

INTERFACE TimeSearcher;
IMPORT BDDCausal, BDDBDDTbl;

TYPE
  T <: PublicTS;

  PublicTS = BDDCausal.T OBJECT METHODS
    init(eqns : BDDBDDTbl.T) : T;
  END;

CONST Brand = "TimeSearcher";

END TimeSearcher.
