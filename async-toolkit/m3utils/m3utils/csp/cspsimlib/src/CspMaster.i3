INTERFACE CspMaster;
IMPORT CspSim;
IMPORT CspRemote;

TYPE
  T <: Public;

  Public = CspRemote.T OBJECT METHODS
    init(nworkers : CARDINAL;
         cmd      : TEXT;
         bld      : CspSim.Builder;
         mt       : CARDINAL        ) : T;
    run();
  END;

CONST Brand = "CspMaster";

END CspMaster.
