INTERFACE TechKill;
IMPORT Pathname, Rd, OSError;

PROCEDURE FromLogFile(logFn : Pathname.T)
  RAISES { Rd.Failure, OSError.E };

END TechKill.
