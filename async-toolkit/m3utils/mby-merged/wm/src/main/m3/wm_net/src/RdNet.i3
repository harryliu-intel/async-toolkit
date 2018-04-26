INTERFACE RdNet;
IMPORT Rd;
IMPORT Thread;
FROM NetTypes IMPORT U8, U16, U32;

PROCEDURE GetU8(rd : Rd.T) : U8
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetU16(rd : Rd.T) : U16
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetU32(rd : Rd.T) : U32
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

CONST Brand = "RdNet";

END RdNet.
