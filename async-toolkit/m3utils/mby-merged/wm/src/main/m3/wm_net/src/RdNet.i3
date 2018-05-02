INTERFACE RdNet;
IMPORT Rd;
IMPORT Thread;
FROM NetTypes IMPORT U8, U16, U32, U64, ByteOrder;
IMPORT NetContext;
IMPORT ServerPacket AS Pkt;

  (**********************************************************************)
  (*                       GENERIC VERSIONS                             *)
  (**********************************************************************)

PROCEDURE GetU8S(s : Pkt.T; at : CARDINAL; bo := ByteOrder.BE) : U8;

PROCEDURE GetU16S(s : Pkt.T; at : CARDINAL; bo := ByteOrder.BE) : U16;

PROCEDURE GetU32S(s : Pkt.T; at : CARDINAL; bo := ByteOrder.BE) : U32;

PROCEDURE GetU64S(s : Pkt.T; at : CARDINAL; bo := ByteOrder.BE) : U64;
  
  (**********************************************************************)
  (*                      BIG-ENDIAN VERSIONS                           *)
  (**********************************************************************)

PROCEDURE GetU8(rd : Rd.T) : U8
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetU16(rd : Rd.T) : U16
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetU32(rd : Rd.T) : U32
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetU64(rd : Rd.T) : U64
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

  (**********************************************************************)
  
PROCEDURE GetU8C(rd : Rd.T; VAR cx : NetContext.T) : U8
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short };

PROCEDURE GetU16C(rd : Rd.T; VAR cx : NetContext.T) : U16
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short };

PROCEDURE GetU32C(rd : Rd.T; VAR cx : NetContext.T) : U32
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short };

PROCEDURE GetU64C(rd : Rd.T; VAR cx : NetContext.T) : U64
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short };

  (**********************************************************************)
  (*                     LITTLE-ENDIAN VERSIONS                         *)
  (**********************************************************************)

PROCEDURE GetU8LE(rd : Rd.T) : U8
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetU16LE(rd : Rd.T) : U16
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetU32LE(rd : Rd.T) : U32
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetU64LE(rd : Rd.T) : U64
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };

  (**********************************************************************)
  
PROCEDURE GetU8CLE(rd : Rd.T; VAR cx : NetContext.T) : U8
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short };

PROCEDURE GetU16CLE(rd : Rd.T; VAR cx : NetContext.T) : U16
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short };

PROCEDURE GetU32CLE(rd : Rd.T; VAR cx : NetContext.T) : U32
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short };

PROCEDURE GetU64CLE(rd : Rd.T; VAR cx : NetContext.T) : U64
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short };

  
CONST Brand = "RdNet";

END RdNet.
