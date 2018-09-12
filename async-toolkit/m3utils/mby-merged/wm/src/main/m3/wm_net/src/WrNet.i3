INTERFACE WrNet;
IMPORT Wr;
IMPORT Thread;
FROM NetTypes IMPORT U8, U16, U32, U64, ByteOrder;
IMPORT NetContext;
IMPORT ServerPacket AS Pkt;

(**********************************************************************)
(*                         GENERIC    WRITES                          *)
(**********************************************************************)

PROCEDURE PutU8G(s : Pkt.T; e : Pkt.End; u : U8; bo := ByteOrder.BE);

PROCEDURE PutU16G(s : Pkt.T; e : Pkt.End; u : U16; bo := ByteOrder.BE);

PROCEDURE PutU32G(s : Pkt.T; e : Pkt.End; u : U32; bo := ByteOrder.BE);

PROCEDURE PutU64G(s : Pkt.T; e : Pkt.End; u : U64; bo := ByteOrder.BE);
  
(**********************************************************************)
(*                         BIG-ENDIAN WRITES                          *)
(**********************************************************************)

PROCEDURE PutU8(wr : Wr.T; u : U8)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU16(wr : Wr.T; u : U16)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU32(wr : Wr.T; u : U32)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU64(wr : Wr.T; u : U64)
  RAISES { Wr.Failure, Thread.Alerted };

(**********************************************************************)

PROCEDURE PutU8C(wr : Wr.T; u : U8; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU16C(wr : Wr.T; u : U16; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU32C(wr : Wr.T; u : U32; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU64C(wr : Wr.T; u : U64; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted };

  (**********************************************************************)

PROCEDURE PutU8S(s : Pkt.T; at : CARDINAL; u : U8);
  
PROCEDURE PutU16S(s : Pkt.T; at : CARDINAL; u : U16);
  
PROCEDURE PutU32S(s : Pkt.T; at : CARDINAL; u : U32);
  
PROCEDURE PutU64S(s : Pkt.T; at : CARDINAL; u : U64);
  
  (**********************************************************************)
  (*                       LITTLE-ENDIAN WRITES                         *)
  (**********************************************************************)

PROCEDURE PutU8LE(wr : Wr.T; u : U8)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU16LE(wr : Wr.T; u : U16)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU32LE(wr : Wr.T; u : U32)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU64LE(wr : Wr.T; u : U64)
  RAISES { Wr.Failure, Thread.Alerted };

  (**********************************************************************)

PROCEDURE PutU8LEC(wr : Wr.T; u : U8; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU16LEC(wr : Wr.T; u : U16; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU32LEC(wr : Wr.T; u : U32; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE PutU64LEC(wr : Wr.T; u : U64; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted };
  
  (**********************************************************************)

PROCEDURE PutU8LES(s : Pkt.T; at : CARDINAL; u : U8);
  
PROCEDURE PutU16LES(s : Pkt.T; at : CARDINAL; u : U16);
  
PROCEDURE PutU32LES(s : Pkt.T; at : CARDINAL; u : U32);
  
PROCEDURE PutU64LES(s : Pkt.T; at : CARDINAL; u : U64);

CONST Brand = "WrNet";

END WrNet.
