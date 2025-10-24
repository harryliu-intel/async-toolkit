INTERFACE SchmoozerResult;
IMPORT Rd, OSError, Pathname;

EXCEPTION Parse;
          
TYPE
  T = OBJECT METHODS
    processOutput(dirName : Pathname.T) RAISES { OSError.E, Rd.Failure, Parse };
  END;

END SchmoozerResult.
