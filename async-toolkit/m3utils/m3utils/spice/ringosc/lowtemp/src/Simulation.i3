INTERFACE Simulation;
IMPORT Pathname, ProcUtils;

TYPE
  T = RECORD
    cmd  : TEXT;
    path : Pathname.T;
    cm   : ProcUtils.Completion;
  END;

CONST Brand = "Simulation";

END Simulation.
