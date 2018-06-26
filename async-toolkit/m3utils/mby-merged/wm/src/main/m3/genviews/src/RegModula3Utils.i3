INTERFACE RegModula3Utils;
IMPORT Wx, Pathname;
IMPORT OSError, Wr, Thread;
IMPORT RdlNum, BigInt;

PROCEDURE CopyWx(READONLY wx : ARRAY OF Wx.T; to : Pathname.T)
  RAISES { OSError.E, Wr.Failure, Thread.Alerted };

PROCEDURE DefVal(canBeNil : RdlNum.T) : BigInt.T;

END RegModula3Utils.
  
