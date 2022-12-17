MODULE ZtraceNodeHeader;
IMPORT Rd, Wr;
IMPORT Thread;
IMPORT ArithConstants;
IMPORT UnsafeWriter, UnsafeReader;

PROCEDURE Write(wr : Wr.T; t : T) 
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    UnsafeWriter.WriteI(wr, t.bytes);
    UnsafeWriter.WriteLRA(wr, ARRAY [0..1] OF LONGREAL { t.min, t.max });
    Wr.PutChar(wr, VAL(t.code, CHAR))
  END Write;

PROCEDURE Read(rd : Rd.T) : T
  RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    t : T;
  BEGIN
    t.bytes := UnsafeReader.ReadI(rd);
    t.min   := UnsafeReader.ReadLR(rd);
    t.max   := UnsafeReader.ReadLR(rd);
    WITH c = Rd.GetChar(rd) DO
      t.code := ORD(c)
    END;
    RETURN t
  END Read;

BEGIN END ZtraceNodeHeader.
