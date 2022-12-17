MODULE ZtraceFile;
IMPORT Rd, Wr;
IMPORT Thread;
IMPORT TraceFile;
IMPORT ZtraceNodeHeader;

PROCEDURE Write(wr : Wr.T; t : T) 
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    TraceFile.WriteHeader(wr, t.header);
    FOR i := FIRST(t.directory^) TO LAST(t.directory^) DO
      ZtraceNodeHeader.Write(wr, t.directory[i])
    END;
    Wr.Flush(wr)
  END Write;

PROCEDURE Read(rd : Rd.T) : T
  RAISES { TraceFile.FormatError, Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    t : T;
  BEGIN
    t.header    := TraceFile.ReadHeader(rd);
    t.directory := NEW(REF Directory, t.header.nwaves);
    FOR i := FIRST(t.directory^) TO LAST(t.directory^) DO
      t.directory[i] := ZtraceNodeHeader.Read(rd)
    END;
    RETURN t
  END Read;

BEGIN END ZtraceFile.
