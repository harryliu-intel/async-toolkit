INTERFACE ArithCoder;
IMPORT ArithCallback;
IMPORT Rd, Thread;

(* shared interface for CO/DEC 

   Note that at present we only support one model, which is that the
   environment drives inputs into the codec, and the codec calls a callback
   for every character it produces.

   We should maybe enhance this to support buffered output, or other 
   output models.
*)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    setCallback(cb : ArithCallback.T);
    char(c : CHAR);
    chars(READONLY a : ARRAY OF CHAR);
    text(t : TEXT);
    eof();
    rdTillEof(rd : Rd.T) RAISES { Rd.Failure, Thread.Alerted };
  END;

CONST Brand = "ArithCoder";

END ArithCoder.
