INTERFACE WrNet;
IMPORT Wr;
IMPORT Thread;
FROM NetTypes IMPORT U8, U16, U32;
     
PROCEDURE PutU8(wr : Wr.T; u : U8)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU16(wr : Wr.T; u : U16)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU32(wr : Wr.T; u : U32)
  RAISES { Wr.Failure, Thread.Alerted };

CONST Brand = "WrNet";

END WrNet.
