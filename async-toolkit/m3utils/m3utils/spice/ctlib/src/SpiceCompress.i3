INTERFACE SpiceCompress;
IMPORT Trace;
IMPORT Matrix;
IMPORT Wr;
IMPORT TripleRefTbl;
IMPORT Rd;
IMPORT PolySegment16Serial;

PROCEDURE DoDemo(targMaxDev : LONGREAL;
                 KK         : REF ARRAY OF CARDINAL;
                 trace      : Trace.T;
                 doAllDumps : BOOLEAN);

TYPE
  Norm = RECORD
    min, max : LONGREAL;
  END;
  
PROCEDURE CompressArray(rn            : TEXT;
                        (* for debug *)
                        
                        VAR darr : ARRAY OF LONGREAL;
                        (* input data---will be normalized in place *)
                        
                        VAR rarr : ARRAY OF LONGREAL;
                        (* workspace *)
                        
                        targMaxDev : LONGREAL;
                        doAllDumps : BOOLEAN;

                        wr : Wr.T;
                        (* can be NIL *)

                        VAR norm   : Norm;

                        mem : TripleRefTbl.T := NIL;

                        doDump := FALSE;
                        (* dump darr and rarr *)
                        )
  
  RAISES { Matrix.Singular };

PROCEDURE DecompressArray(rd       : Rd.T;
                          VAR rarr : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, PolySegment16Serial.Error, Rd.EndOfFile };
  (* returns array of normalized values, expanded to fit.
     normalization must follow a different path. (not handled within this
     format) *)

END SpiceCompress.
