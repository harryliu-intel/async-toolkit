INTERFACE RegModula3Utils;
IMPORT Wx, Pathname;
IMPORT OSError, Wr, Thread;

PROCEDURE CopyWx(READONLY wx : ARRAY OF Wx.T; to : Pathname.T)
  RAISES { OSError.E, Wr.Failure, Thread.Alerted };

END RegModula3Utils.
  
