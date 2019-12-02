MODULE WxToWr;
IMPORT Wx, Wr;
IMPORT FileWr;
IMPORT Thread, OSError;
IMPORT Pathname;

PROCEDURE Single(wx : Wx.T; wr : Wr.T)
  RAISES { Thread.Alerted, Wr.Failure } =
  BEGIN
    Wr.PutText(wr, Wx.ToText(wx))
  END Single;

PROCEDURE Multiple(READONLY in : ARRAY OF Wx.T; out : Wr.T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := FIRST(in) TO LAST(in) DO
      Single(in[i], out)
    END
  END Multiple;

PROCEDURE MultiToFile(READONLY wx : ARRAY OF Wx.T; to : Pathname.T)
  (* copy an array of Wx.Ts in order to an output file *)
  RAISES { OSError.E, Wr.Failure, Thread.Alerted } =
  VAR
    wr := FileWr.Open(to);
  BEGIN
    Multiple(wx, wr);
    Wr.Close(wr)
  END MultiToFile;

BEGIN END WxToWr.
