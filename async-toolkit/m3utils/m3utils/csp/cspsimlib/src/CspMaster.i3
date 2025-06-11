INTERFACE CspMaster;
IMPORT CspSim;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(nworkers : CARDINAL;
         cmd      : TEXT;
         bld      : CspSim.Builder;
         mt       : CARDINAL        ) : T;
    run();
  END;

CONST Brand = "CspMaster";

END CspMaster.
