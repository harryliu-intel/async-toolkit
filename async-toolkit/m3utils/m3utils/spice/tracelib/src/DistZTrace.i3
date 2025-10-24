INTERFACE DistZTrace;
IMPORT Thread, Wr, Matrix;
IMPORT ArithConstants;

(* this defines the format used for single waveforms to be transmitted
   in a stream format (e.g., for a distributed application) *)

PROCEDURE WriteOut(wr            : Wr.T;

                   VAR a         : ARRAY OF LONGREAL;
                   (* a will be normalized in place *)
                   
                   nodeid        : CARDINAL;
                   (* nodeid in trace file whither data is destined *)
                   
                   doDump        : BOOLEAN;
                   
                   relPrec       : LONGREAL;
                   
                   doAllDumps    : BOOLEAN;

                   code          : ArithConstants.Encoding;

                   quick         := FALSE)
  RAISES { Thread.Alerted, Wr.Failure, Matrix.Singular };
  (* this is the counterpart to FsdbComms.ReadCompressedNodeDataG *)

PROCEDURE DoArithCompress(of          : TEXT;
                          VAR codeIdx : ArithConstants.XCodeIdx) : TEXT;
  (* select a codebook and compress the text <of>
     return value is the compressed text
     codeIdx is the ID of the chosen encoding scheme

     we should update this so that codeIdx is used to control the encoding 
     process if not set to Automatic.  If it is set to Automatic,
     this routine should select an encoding, use it, and write it to
     codeIdx.
  *)
     
END DistZTrace.
