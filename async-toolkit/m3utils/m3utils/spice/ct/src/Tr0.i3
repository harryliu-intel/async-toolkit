INTERFACE Tr0;
IMPORT Rd;
IMPORT Pathname;
IMPORT TextSeq;

PROCEDURE Parse(wd, ofn : Pathname.T;
                names : TextSeq.T;
                maxFiles : CARDINAL;
                VAR nFiles : CARDINAL;
                MaxMem : CARDINAL;
                VAR lbp, lbq : CARDINAL;
                VAR lbuff    : REF ARRAY OF ARRAY OF LONGREAL;

                timeScaleFactor,
                timeOffset,
                voltageScaleFactor,
                voltageOffset : LONGREAL;

                dutName : TEXT;
                 
                rd           : Rd.T)
  RAISES { Rd.Failure, ShortRead, SyntaxError };

EXCEPTION ShortRead;

EXCEPTION SyntaxError(TEXT);

END Tr0.
