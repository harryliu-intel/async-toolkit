INTERFACE Fsdb;

IMPORT TextSeqSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT Pathname;
IMPORT Rd;

FROM Tr0 IMPORT ShortRead, SyntaxError;

PROCEDURE Parse(wd            : Pathname.T;
                (* work directory, e.g., ct.work *)
                
                ofn           : Pathname.T;
                (* output ROOT name, e.g., xa *)
                
                names         : TextSeqSeq.T;
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
                maxNodes      : CARDINAL;
                translate, noX: BOOLEAN;
                scopesep      : TEXT;
                cmdPath       : Pathname.T;
                compressPath  : Pathname.T;
                compressPrec  : LONGREAL;
                threads       : CARDINAL;
                interpolate   : LONGREAL;
                maxTime       : LONGREAL)
  RAISES { Rd.Failure, ShortRead, SyntaxError };
  (* 
     when Parse returns, it has left a number of blocked files in the
     working directory wd, which can then be strung together into a single
     large trace file.

     See DataBlock for details on the blocked encoding 

     The TempReader interface consumes the blocked files in the working 
     directory.
  *)
  
CONST NoInterpolate = FIRST(LONGREAL); (* do not interpolate *)
        
END Fsdb.

