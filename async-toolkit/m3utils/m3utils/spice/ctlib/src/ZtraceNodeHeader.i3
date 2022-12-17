INTERFACE ZtraceNodeHeader;

(* the header corresponding to a single simulation node, written to the
   directory of a ztrace file *)

FROM TraceFile IMPORT UInt32;
IMPORT Rd, Wr;
IMPORT Thread;
IMPORT ArithConstants;

TYPE
  T = RECORD
    bytes    : UInt32;   (* bytes taken by data on disk *)
    min, max : LONGREAL; (* min and max values of node *)
    code     : ArithConstants.Encoding;
  END;

CONST Brand = "ZtraceNodeHeader";

PROCEDURE Write(wr : Wr.T; t : T) 
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE Read(rd : Rd.T) : T
    RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted };

END ZtraceNodeHeader.
