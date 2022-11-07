INTERFACE Fsdb;

IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT Pathname;
IMPORT Rd;

FROM Tr0 IMPORT ShortRead, SyntaxError;

PROCEDURE Parse(wd            : Pathname.T;
                (* work directory, e.g., ct.work *)
                
                ofn           : Pathname.T;
                (* output ROOT name, e.g., xa *)
                
                names         : TextSeq.T;
                (* sequence of canonical names *)
                
                maxFiles      : CARDINAL;
                VAR nFiles    : CARDINAL;

                timeScaleFactor,
                timeOffset,
                voltageScaleFactor,
                voltageOffset : LONGREAL;

                dutName       : TEXT;
                 
                fsdbPath      : Pathname.T;
                wait          : BOOLEAN;
                restrictNodes : TextSet.T;
                restrictRegEx : RegExList.T;
                cmdPath       : Pathname.T;
                threads       : CARDINAL;
                interpolate   : LONGREAL)
  RAISES { Rd.Failure, ShortRead, SyntaxError };

CONST NoInterpolate = FIRST(LONGREAL); (* do not interpolate *)
        
END Fsdb.

