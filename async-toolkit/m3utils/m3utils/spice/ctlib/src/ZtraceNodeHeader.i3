INTERFACE ZtraceNodeHeader;

(* the header corresponding to a single simulation node, written to the
   directory of a ztrace file *)

FROM TraceFile IMPORT UInt32;
IMPORT Rd, Wr;
IMPORT Thread;
IMPORT ArithConstants;
IMPORT SpiceCompress;
IMPORT Word;

TYPE
  UInt64 = Word.T;
  
  T = RECORD
    bytes    : UInt32;
    (* bytes taken by data on disk *)

    start    : UInt64;
    (* start byte of object *)

    norm     : SpiceCompress.Norm;
    (* min and max values of node -- may not be valid for dense data *)
    
    code     : ArithConstants.Encoding;

    decimate : [0..255] := 0;
  END;

CONST SerialSize = 4 + 8 + SpiceCompress.NormSerialSize + 1 + 1;

CONST Brand = "ZtraceNodeHeader";

PROCEDURE Write(wr : Wr.T; t : T) 
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE Read(rd : Rd.T) : T
    RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted };

PROCEDURE Format(t : T) : TEXT;

PROCEDURE CompareByStart(READONLY a, b : T) : [-1..1];

CONST Compare = CompareByStart;
  
END ZtraceNodeHeader.
