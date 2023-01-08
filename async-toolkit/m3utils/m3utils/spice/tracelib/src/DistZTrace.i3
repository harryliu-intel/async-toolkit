INTERFACE DistZTrace;
IMPORT Thread, Wr, Matrix;

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

                   noArith       : BOOLEAN)
  RAISES { Thread.Alerted, Wr.Failure, Matrix.Singular };
  (* this is the counterpart to FsdbComms.ReadCompressedNodeDataG *)

END DistZTrace.
