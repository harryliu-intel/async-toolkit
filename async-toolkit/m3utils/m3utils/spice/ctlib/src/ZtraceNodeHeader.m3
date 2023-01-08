MODULE ZtraceNodeHeader;
IMPORT Rd, Wr;
IMPORT Thread;
IMPORT UnsafeWriter, UnsafeReader;
FROM Fmt IMPORT F, Int, Unsigned;
IMPORT SpiceCompress;

PROCEDURE Write(wr : Wr.T; t : T) 
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    UnsafeWriter.WriteI(wr, t.bytes);
    UnsafeWriter.WriteU64(wr, t.start);
    UnsafeWriter.WriteLRA(wr, ARRAY [0..1] OF LONGREAL { t.norm.min, t.norm.max });
    Wr.PutChar(wr, VAL(t.code, CHAR));
    Wr.PutChar(wr, VAL(t.decimate, CHAR))
  END Write;

PROCEDURE Read(rd : Rd.T) : T
  RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    t : T;
  BEGIN
    t.bytes      := UnsafeReader.ReadI(rd);
    t.start      := UnsafeReader.ReadU64(rd);
    t.norm.min   := UnsafeReader.ReadLR(rd);
    t.norm.max   := UnsafeReader.ReadLR(rd);
    WITH c = Rd.GetChar(rd) DO
      t.code := ORD(c)
    END;
    WITH c = Rd.GetChar(rd) DO
      t.decimate := ORD(c)
    END;
    RETURN t
  END Read;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN F("{bytes=%s start=%s norm=%s code=%s decimate=%s}",
             Int(t.bytes),
             Unsigned(t.start, base := 10),
             SpiceCompress.FormatNorm(t.norm),
             Int(t.code),
             Int(t.decimate))
  END Format;

BEGIN END ZtraceNodeHeader.
