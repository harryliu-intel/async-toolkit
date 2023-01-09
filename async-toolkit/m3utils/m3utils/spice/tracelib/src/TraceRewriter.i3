INTERFACE TraceRewriter;
IMPORT TextSeq;
IMPORT Pathname;
IMPORT TraceOp;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(root : Pathname.T; rewriterPath : Pathname.T) : T;

    addhi(stream          : TEXT;
          aliases         : TextSeq.T);
    (* 
       given the data stream "data", in the same format as that produced
       by spicestream in Filter mode (see spicestream/src/Main.m3), add
       as the highest node id in the file in the appropriate format.

       aliases given in the aliases structure.  Do not include the 
       NAMES alias---this code will add it automatically 
    *)

    sync();
    (* flush all edits to disk *)

    addhiOp(op : TraceOp.T; aliases : TextSeq.T);
  END;

CONST
  Brand = "TraceRewriter";

END TraceRewriter.
