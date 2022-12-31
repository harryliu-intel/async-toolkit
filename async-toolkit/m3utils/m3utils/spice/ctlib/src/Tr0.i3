INTERFACE Tr0;
IMPORT Rd;
IMPORT Pathname;
IMPORT TextSeqSeq;
IMPORT TextSet;
IMPORT TextSeq;
IMPORT RegExList;

PROCEDURE Parse(wd,                            (* working directory       *)
                ofn           : Pathname.T;    (* output filename (root)  *)
                names         : TextSeqSeq.T;  (* names sequence          *)

                maxFiles      : CARDINAL;      (* max intermediate files  *)
                VAR nFiles    : CARDINAL;      (* act. intermediate files *)
                MaxMem        : CARDINAL;      (* max memory to use       *)

                timeScaleFactor,            
                timeOffset,
                voltageScaleFactor,
                voltageOffset : LONGREAL;

                dutName       : TEXT;
                 
                rd            : Rd.T;
                wait          : BOOLEAN;
                restrictNodes : TextSet.T;  (* nodes to process             *)
                restrictRegEx : RegExList.T;(* regex to match nodes against *)
                maxNodes      : CARDINAL;
                translate     : BOOLEAN
  )
  RAISES { Rd.Failure, ShortRead, SyntaxError };

EXCEPTION ShortRead;

EXCEPTION SyntaxError(TEXT);

PROCEDURE RenameBack(dutName, txt : TEXT) : TEXT;

PROCEDURE Seq1(txt : TEXT) : TextSeq.T;

END Tr0.
