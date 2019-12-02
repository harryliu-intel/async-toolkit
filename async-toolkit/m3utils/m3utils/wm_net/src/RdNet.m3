MODULE RdNet;
IMPORT Rd;
IMPORT Word;
IMPORT Thread;
FROM NetTypes IMPORT U8, U16, U32, U64, ByteOrder;
IMPORT NetContext;
IMPORT ServerPacket AS Pkt;

PROCEDURE GetU8(rd : Rd.T) : U8 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U8 := 0;
  BEGIN
    FOR i := 8-8 TO 0 BY -8 DO
      res := GetByte(rd, res, i)
    END;
    RETURN res
  END GetU8;

PROCEDURE GetU16(rd : Rd.T) : U16 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U16 := 0;
  BEGIN
    FOR i := 16-8 TO 0 BY -8 DO
      res := GetByte(rd, res, i)
    END;
    RETURN res
  END GetU16;

PROCEDURE GetU32(rd : Rd.T) : U32 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U32 := 0;
  BEGIN
    FOR i := 32-8 TO 0 BY -8 DO
      res := GetByte(rd, res, i)
    END;
    RETURN res
  END GetU32;

PROCEDURE GetU64(rd : Rd.T) : U64 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U64 := 0;
  BEGIN
    FOR i := 64-8 TO 0 BY -8 DO
      res := GetByte(rd, res, i)
    END;
    RETURN res
  END GetU64;
  
  (**********************************************************************)

PROCEDURE GetU8C(rd : Rd.T; VAR cx : NetContext.T) : U8 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short } =
  VAR
    res : U8 := 0;
  BEGIN
    FOR i := 8-8 TO 0 BY -8 DO
      res := GetByteC(rd, res, i, cx)
    END;
    RETURN res
  END GetU8C;

PROCEDURE GetU16C(rd : Rd.T; VAR cx : NetContext.T) : U16 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short } =
  VAR
    res : U16 := 0;
  BEGIN
    FOR i := 16-8 TO 0 BY -8 DO
      res := GetByteC(rd, res, i, cx)
    END;
    RETURN res
  END GetU16C;

PROCEDURE GetU32C(rd : Rd.T; VAR cx : NetContext.T) : U32 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short } =
  VAR
    res : U32 := 0;
  BEGIN
    FOR i := 32-8 TO 0 BY -8 DO
      res := GetByteC(rd, res, i, cx)
    END;
    RETURN res
  END GetU32C;

PROCEDURE GetU64C(rd : Rd.T; VAR cx : NetContext.T) : U64 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short } =
  VAR
    res : U64 := 0;
  BEGIN
    FOR i := 64-8 TO 0 BY -8 DO
      res := GetByteC(rd, res, i, cx)
    END;
    RETURN res
  END GetU64C;
  

  (**********************************************************************)
  (*                     LITTLE-ENDIAN VERSIONS                         *)
  (**********************************************************************)

PROCEDURE GetU8LE(rd : Rd.T) : U8 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U8 := 0;
  BEGIN
    FOR i := 0 TO  8-8 BY 8 DO
      res := GetByte(rd, res, i)
    END;
    RETURN res
  END GetU8LE;

PROCEDURE GetU16LE(rd : Rd.T) : U16 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U16 := 0;
  BEGIN
    FOR i := 0 TO  16-8 BY 8 DO
      res := GetByte(rd, res, i)
    END;
    RETURN res
  END GetU16LE;

PROCEDURE GetU32LE(rd : Rd.T) : U32 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U32 := 0;
  BEGIN
    FOR i := 0 TO  32-8 BY 8 DO
      res := GetByte(rd, res, i)
    END;
    RETURN res
  END GetU32LE;

PROCEDURE GetU64LE(rd : Rd.T) : U64 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U64 := 0;
  BEGIN
    FOR i := 0 TO  64-8 BY 8 DO
      res := GetByte(rd, res, i)
    END;
    RETURN res
  END GetU64LE;
  
  
  (**********************************************************************)

PROCEDURE GetU8CLE(rd : Rd.T; VAR cx : NetContext.T) : U8 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short } =
  VAR
    res : U8 := 0;
  BEGIN
    FOR i := 0 TO  8-8 BY 8 DO
      res := GetByteC(rd, res, i, cx)
    END;
    RETURN res
  END GetU8CLE;

PROCEDURE GetU16CLE(rd : Rd.T; VAR cx : NetContext.T) : U16 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short } =
  VAR
    res : U16 := 0;
  BEGIN
    FOR i := 0 TO  16-8 BY 8 DO
      res := GetByteC(rd, res, i, cx)
    END;
    RETURN res
  END GetU16CLE;

PROCEDURE GetU32CLE(rd : Rd.T; VAR cx : NetContext.T) : U32 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short } =
  VAR
    res : U32 := 0;
  BEGIN
    FOR i := 0 TO  32-8 BY 8 DO
      res := GetByteC(rd, res, i, cx)
    END;
    RETURN res
  END GetU32CLE;

PROCEDURE GetU64CLE(rd : Rd.T; VAR cx : NetContext.T) : U64 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short } =
  VAR
    res : U64 := 0;
  BEGIN
    FOR i := 0 TO  64-8 BY 8 DO
      res := GetByteC(rd, res, i, cx)
    END;
    RETURN res
  END GetU64CLE;
  
  (**********************************************************************)

PROCEDURE GetU8S(s : Pkt.T; VAR at : CARDINAL; VAR res : U8; bo : ByteOrder) : BOOLEAN =
  VAR
    k := 0;
  BEGIN
    IF s.size() < 1 THEN RETURN FALSE END;
    res := 0;
    CASE bo OF
      ByteOrder.BE =>
      FOR i :=   8-8 TO 0 BY  -8 DO
        res := GetByteS(s, at+k, res, i); INC(k)
      END
    |
      ByteOrder.LE =>
      FOR i :=   0 TO 8-8 BY  8 DO
        res := GetByteS(s, at+k, res, i); INC(k)
      END
    END;
    INC(at,1);
    RETURN TRUE
  END GetU8S;

PROCEDURE GetU16S(s : Pkt.T; VAR at : CARDINAL; VAR res : U16; bo : ByteOrder) : BOOLEAN =
  VAR
    k := 0;
  BEGIN
    IF s.size() < 2 THEN RETURN FALSE END;
    res := 0;
    CASE bo OF
      ByteOrder.BE =>
      FOR i :=   16-8 TO 0 BY  -8 DO
        res := GetByteS(s, at+k, res, i); INC(k)
      END
    |
      ByteOrder.LE =>
      FOR i :=   0 TO 16-8 BY  8 DO
        res := GetByteS(s, at+k, res, i); INC(k)
      END
    END;
    INC(at,2);
    RETURN TRUE
  END GetU16S;

PROCEDURE GetU32S(s : Pkt.T; VAR at : CARDINAL; VAR res : U32; bo : ByteOrder) : BOOLEAN =
  VAR
    k := 0;
  BEGIN
    IF s.size() < 4 THEN RETURN FALSE END;
    res := 0;
    CASE bo OF
      ByteOrder.BE =>
      FOR i :=   32-8 TO 0 BY  -8 DO
        res := GetByteS(s, at+k, res, i); INC(k)
      END
    |
      ByteOrder.LE =>
      FOR i :=   0 TO 32-8 BY  8 DO
        res := GetByteS(s, at+k, res, i); INC(k)
      END
    END;
    INC(at,4);
    RETURN TRUE
  END GetU32S;

PROCEDURE GetU64S(s : Pkt.T; VAR at : CARDINAL; VAR res : U64; bo : ByteOrder) : BOOLEAN =
  VAR
    k := 0;
  BEGIN
    IF s.size() < 8 THEN RETURN FALSE END;
    res := 0;
    CASE bo OF
      ByteOrder.BE =>
      FOR i :=   64-8 TO 0 BY  -8 DO
        res := GetByteS(s, at+k, res, i); INC(k)
      END
    |
      ByteOrder.LE =>
      FOR i :=   0 TO 64-8 BY  8 DO
        res := GetByteS(s, at+k, res, i); INC(k)
      END
    END;
    INC(at,8);
    RETURN TRUE
  END GetU64S;

  (**********************************************************************)

PROCEDURE GetByte(rd              : Rd.T;
                  READONLY w      : Word.T;
                  b               : [0..BITSIZE(Word.T)-8]) : Word.T
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    c : [0..16_ff];
  BEGIN
    c := ORD(Rd.GetChar(rd));
    RETURN Word.Insert(w, c, b, 8)
  END GetByte;

PROCEDURE GetByteC(rd              : Rd.T;
                   READONLY w      : Word.T;
                   b               : [0..BITSIZE(Word.T)-8];
                   VAR cx          : NetContext.T) : Word.T
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted, NetContext.Short } =
  VAR
    c : [0..16_ff];
  BEGIN
    IF cx.rem = 0 THEN RAISE NetContext.Short END;
    DEC(cx.rem);
    c := ORD(Rd.GetChar(rd));
    RETURN Word.Insert(w, c, b, 8)
  END GetByteC;

PROCEDURE GetByteS(s               : Pkt.T;
                   i               : CARDINAL;
                   READONLY w      : Word.T;
                   b               : [0..BITSIZE(Word.T)-8]) : Word.T =
  VAR
    c : [0..16_ff];
  BEGIN
    c := ORD(Pkt.Get(s,i));
    RETURN Word.Insert(w, c, b, 8)
  END GetByteS;

BEGIN END RdNet.
