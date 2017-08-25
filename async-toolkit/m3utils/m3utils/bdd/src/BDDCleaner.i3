(* $Id: BDDCleaner.i3,v 1.1 2014/11/20 10:30:40 mika Exp $ *)

INTERFACE BDDCleaner;
IMPORT BDD, BDDSystemState;

TYPE
  Cleaner <: PublicCleaner;

  PublicCleaner = OBJECT METHODS
    init() : Cleaner;

    state() : BDDSystemState.SystemState;

    clean(b : BDD.T) : BDD.T;
    (* takes BDD b in another system state and rebuilds it in the cleaner's
       state *)
  END;

END BDDCleaner.
