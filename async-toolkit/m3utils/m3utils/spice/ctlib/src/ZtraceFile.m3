MODULE ZtraceFile;
IMPORT Rd, Wr;
IMPORT Thread;
IMPORT TraceFile;
IMPORT ZtraceNodeHeader;
IMPORT UnsafeWriter;
IMPORT UnsafeReader;
IMPORT Wx;
IMPORT Debug;
FROM Fmt IMPORT F, Int;

PROCEDURE Write(wr : Wr.T; VAR t : T) 
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    TraceFile.WriteHeader(wr, t.header);
    t.dirStart := Wr.Index(wr);
    RewriteDirectory(wr, t);
    UnsafeWriter.WriteI(wr, t.nsteps)
  END Write;

PROCEDURE Read(rd : Rd.T) : T
  RAISES { TraceFile.FormatError, Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    t : T;
  BEGIN
    (* must assume we are at start of file here *)
    t.header    := TraceFile.ReadHeader(rd);
    t.dirStart  := Rd.Index(rd);
    t.directory := NEW(Directory).init();
    FOR i := 0 TO t.header.nwaves - 1 DO
      t.directory.addhi(ZtraceNodeHeader.Read(rd))
    END;
    t.nsteps     := UnsafeReader.ReadI(rd);
    RETURN t
  END Read;

PROCEDURE RewriteDirectory(wr : Wr.T; READONLY t : T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    <*ASSERT t.dirStart # LAST(CARDINAL)*>
    Debug.Out(F("ZtraceFile.RewriteDirectory : t.dirStart = %s",
                Int(t.dirStart)));

    Debug.Out(Format(t));
    
    Wr.Seek(wr, t.dirStart);
    FOR i := 0 TO t.directory.size() - 1 DO
      ZtraceNodeHeader.Write(wr, t.directory.get(i))
    END;
    Wr.Flush(wr)
  END RewriteDirectory;

PROCEDURE Format(READONLY t : T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    Wx.PutText(wx, "<ZtraceFile " & TraceFile.FormatHeader(t.header) & "\n");
    Wx.PutText(wx, F("dirStart %s\n", Int(t.dirStart)));
    FOR i := 0 TO t.directory.size() - 1 DO
      Wx.PutText(wx, F("dir[%s] : ", Int(i)));
      Wx.PutText(wx, ZtraceNodeHeader.Format(t.directory.get(i)));
      Wx.PutChar(wx, '\n')
    END;
    Wx.PutText(wx, F("nsteps %s>\n", Int(t.nsteps)));
    RETURN Wx.ToText(wx)
  END Format;
  
BEGIN END ZtraceFile.
