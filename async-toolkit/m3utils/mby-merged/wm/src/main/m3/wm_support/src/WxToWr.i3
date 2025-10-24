INTERFACE WxToWr;
IMPORT Wx;
IMPORT Wr, Thread, OSError;
IMPORT Pathname;

PROCEDURE Single(in : Wx.T; out : Wr.T) RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE Multiple(READONLY in : ARRAY OF Wx.T; out : Wr.T) RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE MultiToFile(READONLY wx : ARRAY OF Wx.T; to : Pathname.T)
  RAISES { OSError.E, Wr.Failure, Thread.Alerted };

CONST Brand = "WxToWr";

END WxToWr.
