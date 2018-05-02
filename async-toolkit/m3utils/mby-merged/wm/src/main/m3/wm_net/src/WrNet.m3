MODULE WrNet;
IMPORT Wr;
IMPORT Word;
IMPORT Thread;
FROM NetTypes IMPORT U8, U16, U32, U64, ByteOrder;
IMPORT NetContext;
IMPORT ServerPacket AS Pkt;

CONST Dir = ARRAY Pkt.End OF ARRAY ByteOrder OF [-1..1]
  { ARRAY ByteOrder OF [-1..1] {  1, -1 },
    ARRAY ByteOrder OF [-1..1] { -1,  1 } } ;

PROCEDURE PutU8G(s : Pkt.T; e : Pkt.End; u : U8; bo : ByteOrder) =
  BEGIN
    CASE Dir[e][bo] OF
       1 =>
       FOR i :=   0 TO 8-8 BY  8 DO
         PutByteE(s, e, u, i)
       END
    |
      -1 =>
       FOR i := 8-8 TO 0   BY -8 DO
         PutByteE(s, e, u, i)
       END
    |
       0 => <*ASSERT FALSE*>
    END
  END PutU8G;

PROCEDURE PutU16G(s : Pkt.T; e : Pkt.End; u : U16; bo : ByteOrder) =
  BEGIN
    CASE Dir[e][bo] OF
       1 =>
       FOR i :=   0 TO 16-8 BY  8 DO
         PutByteE(s, e, u, i)
       END
    |
      -1 =>
       FOR i := 16-8 TO 0   BY -8 DO
         PutByteE(s, e, u, i)
       END
    |
       0 => <*ASSERT FALSE*>
    END
  END PutU16G;

PROCEDURE PutU32G(s : Pkt.T; e : Pkt.End; u : U32; bo : ByteOrder) =
  BEGIN
    CASE Dir[e][bo] OF
       1 =>
       FOR i :=   0 TO 32-8 BY  8 DO
         PutByteE(s, e, u, i)
       END
    |
      -1 =>
       FOR i := 32-8 TO 0   BY -8 DO
         PutByteE(s, e, u, i)
       END
    |
       0 => <*ASSERT FALSE*>
    END
  END PutU32G;

PROCEDURE PutU64G(s : Pkt.T; e : Pkt.End; u : U64; bo : ByteOrder) =
  BEGIN
    CASE Dir[e][bo] OF
       1 =>
       FOR i :=   0 TO 64-8 BY  8 DO
         PutByteE(s, e, u, i)
       END
    |
      -1 =>
       FOR i := 64-8 TO 0   BY -8 DO
         PutByteE(s, e, u, i)
       END
    |
       0 => <*ASSERT FALSE*>
    END
  END PutU64G;

  (**********************************************************************)

PROCEDURE PutU8(wr : Wr.T; u : U8)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 8-8 TO 0 BY -8 DO
      PutByte(wr, u, i);
    END
  END PutU8;

PROCEDURE PutU16(wr : Wr.T; u : U16)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 16-8 TO 0 BY -8 DO
      PutByte(wr, u, i);
    END
  END PutU16;

PROCEDURE PutU32(wr : Wr.T; u : U32)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 32-8 TO 0 BY -8 DO
      PutByte(wr, u, i);
    END
  END PutU32;

PROCEDURE PutU64(wr : Wr.T; u : U64)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 64-8 TO 0 BY -8 DO
      PutByte(wr, u, i);
    END
  END PutU64;

  (**********************************************************************)

PROCEDURE PutU8C(wr : Wr.T; u : U8; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 8-8 TO 0 BY -8 DO
      PutByteC(wr, u, i, cx);
    END
  END PutU8C;

PROCEDURE PutU16C(wr : Wr.T; u : U16; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 16-8 TO 0 BY -8 DO
      PutByteC(wr, u, i, cx);
    END
  END PutU16C;

PROCEDURE PutU32C(wr : Wr.T; u : U32; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 32-8 TO 0 BY -8 DO
      PutByteC(wr, u, i, cx);
    END
  END PutU32C;

PROCEDURE PutU64C(wr : Wr.T; u : U64; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 64-8 TO 0 BY -8 DO
      PutByteC(wr, u, i, cx);
    END
  END PutU64C;

  (**********************************************************************)

PROCEDURE PutU8S(s : Pkt.T; at : CARDINAL; u : U8) =
  VAR k := 0; BEGIN
    FOR i := 8-8 TO 0 BY -8 DO
      PutByteS(s, at+k, u, i); INC(k)
    END
  END PutU8S;

PROCEDURE PutU16S(s : Pkt.T; at : CARDINAL; u : U16) =
  VAR k := 0; BEGIN
    FOR i := 16-8 TO 0 BY -8 DO
      PutByteS(s, at+k, u, i); INC(k)
    END
  END PutU16S;

PROCEDURE PutU32S(s : Pkt.T; at : CARDINAL; u : U32) =
  VAR k := 0; BEGIN
    FOR i := 32-8 TO 0 BY -8 DO
      PutByteS(s, at+k, u, i); INC(k)
    END
  END PutU32S;

PROCEDURE PutU64S(s : Pkt.T; at : CARDINAL; u : U64) =
  VAR k := 0; BEGIN
    FOR i := 64-8 TO 0 BY -8 DO
      PutByteS(s, at+k, u, i); INC(k)
    END
  END PutU64S;

  (**********************************************************************)
  (*                     LITTLE-ENDIAN VERSIONS                         *)
  (**********************************************************************)

PROCEDURE PutU8LE(wr : Wr.T; u : U8)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 0 TO 8-8 BY 8 DO
      PutByte(wr, u, i);
    END
  END PutU8LE;

PROCEDURE PutU16LE(wr : Wr.T; u : U16)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 0 TO 16-8 BY 8 DO
      PutByte(wr, u, i);
    END
  END PutU16LE;

PROCEDURE PutU32LE(wr : Wr.T; u : U32)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 0 TO 32-8 BY 8 DO
      PutByte(wr, u, i);
    END
  END PutU32LE;

PROCEDURE PutU64LE(wr : Wr.T; u : U64)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 0 TO 64-8 BY 8 DO
      PutByte(wr, u, i);
    END
  END PutU64LE;

  (**********************************************************************)

PROCEDURE PutU8LEC(wr : Wr.T; u : U8; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 0 TO 8-8 BY 8 DO
      PutByteC(wr, u, i, cx);
    END
  END PutU8LEC;

PROCEDURE PutU16LEC(wr : Wr.T; u : U16; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 0 TO 16-8 BY 8 DO
      PutByteC(wr, u, i, cx);
    END
  END PutU16LEC;

PROCEDURE PutU32LEC(wr : Wr.T; u : U32; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 0 TO 32-8 BY 8 DO
      PutByteC(wr, u, i, cx);
    END
  END PutU32LEC;

PROCEDURE PutU64LEC(wr : Wr.T; u : U64; VAR cx : NetContext.T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    FOR i := 0 TO 64-8 BY 8 DO
      PutByteC(wr, u, i, cx);
    END
  END PutU64LEC;

  (**********************************************************************)

PROCEDURE PutU8LES(s : Pkt.T; at : CARDINAL; u : U8) =
  VAR k := 0; BEGIN
    FOR i := 0 TO 8-8 BY 8 DO
      PutByteS(s, at+k, u, i); INC(k)
    END
  END PutU8LES;

PROCEDURE PutU16LES(s : Pkt.T; at : CARDINAL; u : U16) =
  VAR k := 0; BEGIN
    FOR i := 0 TO 16-8 BY 8 DO
      PutByteS(s, at+k, u, i); INC(k)
    END
  END PutU16LES;

PROCEDURE PutU32LES(s : Pkt.T; at : CARDINAL; u : U32) =
  VAR k := 0; BEGIN
    FOR i := 0 TO 32-8 BY 8 DO
      PutByteS(s, at+k, u, i); INC(k)
    END
  END PutU32LES;

PROCEDURE PutU64LES(s : Pkt.T; at : CARDINAL; u : U64) =
  VAR k := 0; BEGIN
    FOR i := 0 TO 64-8 BY 8 DO
      PutByteS(s, at+k, u, i); INC(k)
    END
  END PutU64LES;

  (**********************************************************************)

PROCEDURE PutByte(wr              : Wr.T;
                  READONLY w      : Word.T;
                  b               : [0..BITSIZE(Word.T)-8]) 
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    Wr.PutChar(wr, VAL(Word.Extract(w, b, 8),CHAR))
  END PutByte;

PROCEDURE PutByteC(wr              : Wr.T;
                   READONLY w      : Word.T;
                   b               : [0..BITSIZE(Word.T)-8];
                   VAR cx          : NetContext.T) 
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    Wr.PutChar(wr, VAL(Word.Extract(w, b, 8),CHAR));
    INC(cx.rem)
  END PutByteC;

PROCEDURE PutByteS(s               : Pkt.T;
                   i               : CARDINAL;
                   READONLY w      : Word.T;
                   b               : [0..BITSIZE(Word.T)-8]) =
  BEGIN
    Pkt.Put(s, i, VAL(Word.Extract(w, b, 8),CHAR))
  END PutByteS;

PROCEDURE PutByteE(s               : Pkt.T;
                   e               : Pkt.End;
                   READONLY w      : Word.T;
                   b               : [0..BITSIZE(Word.T)-8]) =
  BEGIN
    Pkt.PutE(s, e, VAL(Word.Extract(w, b, 8),CHAR))
  END PutByteE;

BEGIN END WrNet.
