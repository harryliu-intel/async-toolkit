INTERFACE ZtraceFile;
(* structure of a .ztrace file *)

IMPORT TraceFile;
IMPORT ZtraceNodeHeader AS NodeHeader;
IMPORT Rd, Wr;
IMPORT Thread;

TYPE
  Header = TraceFile.Header;

  Directory = ARRAY OF NodeHeader.T;

  T = Metadata;

  Metadata = RECORD
    (* the collected metadata of a ZtraceFile *)
    header    : Header;
    dirStart  : CARDINAL := LAST(CARDINAL); (* start of directory in bytes *)
    directory : REF Directory;
  END;

  (* 
     file consists of this data plus the node waveform data following it 
     in sequence, packed

     If the format is coded ArithConstants.DenseCode, then the bytes are
     packed floats, as in aspice.

     If the format is coded anything else, it is arithmetically coded 
     polynomially compressed waveform data.
  *)
  
CONST Brand = "ZtraceFile";

PROCEDURE Write(wr : Wr.T; VAR t : T) 
  RAISES { Wr.Failure, Thread.Alerted };
  (* updates dirStart *)

PROCEDURE Read(rd : Rd.T) : T
  RAISES { TraceFile.FormatError, Rd.Failure, Rd.EndOfFile, Thread.Alerted };

PROCEDURE RewriteDirectory(wr : Wr.T; READONLY t : T)
  RAISES { Wr.Failure, Thread.Alerted };

END ZtraceFile.
