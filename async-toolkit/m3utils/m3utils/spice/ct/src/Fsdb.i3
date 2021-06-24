INTERFACE Fsdb;

IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT Pathname;
IMPORT Rd;

FROM Tr0 IMPORT ShortRead, SyntaxError;

PROCEDURE Parse(wd, ofn       : Pathname.T;
                names         : TextSeq.T;
                maxFiles      : CARDINAL;
                VAR nFiles    : CARDINAL;
                MaxMem        : CARDINAL;

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
                threads       : CARDINAL )
  RAISES { Rd.Failure, ShortRead, SyntaxError };

END Fsdb.
