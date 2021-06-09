INTERFACE Tr0;
IMPORT Rd;
IMPORT Pathname;
IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;

PROCEDURE Parse(wd, ofn : Pathname.T;
                names : TextSeq.T;
                maxFiles : CARDINAL;
                VAR nFiles : CARDINAL;
                MaxMem : CARDINAL;

                timeScaleFactor,
                timeOffset,
                voltageScaleFactor,
                voltageOffset : LONGREAL;

                dutName : TEXT;
                 
                rd           : Rd.T;
                wait         : BOOLEAN;
                restrictNodes : TextSet.T;
                restrictRegEx : RegExList.T)
  RAISES { Rd.Failure, ShortRead, SyntaxError };

EXCEPTION ShortRead;

EXCEPTION SyntaxError(TEXT);

PROCEDURE FileIndex(nFiles, nNodes, nodeIndex : CARDINAL) : CARDINAL;

END Tr0.
