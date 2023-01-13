INTERFACE TraceRewriter;
IMPORT TextSeq;
IMPORT Pathname;
IMPORT TraceOp;
IMPORT Rd;
IMPORT OSError;
IMPORT SpiceCompress;
IMPORT ArithConstants;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(root         : Pathname.T;
         rewriterPath : Pathname.T) : T
    RAISES { OSError.E, Rd.Failure, Rd.EndOfFile };
    (* rewriterPath not currently used for anything *)

    addhi(stream          : TEXT;
          norm            : SpiceCompress.Norm;
          code            : ArithConstants.Encoding;
          aliases         : TextSeq.T);
    (* 
       given the data stream "data", in the same format as that produced
       by spicestream in Filter mode (see spicestream/src/Main.m3), add
       as the highest node id in the file in the appropriate format.

       aliases given in the aliases structure.  Do not include the 
       NAMES alias---this code will add it automatically 
    *)

    flush();
    (* flush all edits to disk *)

    addhiOp(op           : TraceOp.T;
            aliases      : TextSeq.T;
            relPrec      : LONGREAL;
            noArith      : BOOLEAN) RAISES { Rd.EndOfFile, Rd.Failure };
  END;

CONST
  Brand = "TraceRewriter";

END TraceRewriter.
