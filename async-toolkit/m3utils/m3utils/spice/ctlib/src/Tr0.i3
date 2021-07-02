INTERFACE Tr0;
IMPORT Rd;
IMPORT Pathname;
IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;

PROCEDURE Parse(wd,                         (* working directory       *)
                ofn           : Pathname.T; (* output filename (root)  *)
                names         : TextSeq.T;  (* names sequence          *)

                maxFiles      : CARDINAL;   (* max intermediate files  *)
                VAR nFiles    : CARDINAL;   (* act. intermediate files *)
                MaxMem        : CARDINAL;   (* max memory to use       *)

                timeScaleFactor,            
                timeOffset,
                voltageScaleFactor,
                voltageOffset : LONGREAL;

                dutName       : TEXT;
                 
                rd            : Rd.T;
                wait          : BOOLEAN;
                restrictNodes : TextSet.T;  (* nodes to process             *)
                restrictRegEx : RegExList.T (* regex to match nodes against *)
  )
  RAISES { Rd.Failure, ShortRead, SyntaxError };

EXCEPTION ShortRead;

EXCEPTION SyntaxError(TEXT);

END Tr0.
