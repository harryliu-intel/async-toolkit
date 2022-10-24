INTERFACE SimWatcher;
IMPORT Thread;
IMPORT ProcUtils;
IMPORT Pathname;
IMPORT TechConfig;

TYPE
  T <: Public;

  Public = Thread.Closure OBJECT
    c       : TechConfig.T;
    cm      : ProcUtils.Completion;
    simRoot : Pathname.T;
    myKill  : REF BOOLEAN; (* set to TRUE if killing *)
  END;

END SimWatcher.
