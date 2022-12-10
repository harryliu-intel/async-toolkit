MODULE Rep16Stream;
IMPORT Rep16;
IMPORT Rd, Wr;
IMPORT UnsafeReader, UnsafeWriter;
IMPORT Thread;
IMPORT Word;
FROM Rep16 IMPORT Signed, Unsigned, OrderBits, Zero;
IMPORT Fmt; FROM Fmt IMPORT Int, F;
IMPORT Debug;

PROCEDURE Bytes(t : Rep16.T) : CARDINAL =
  BEGIN
    IF t.order = 0 THEN
      RETURN (1 + 1) * 2
    ELSE
      RETURN (1 + t.order) * 2
    END
  END Bytes;

PROCEDURE WriteUnsigned(wr : Wr.T; x : Unsigned)
  RAISES { Wr.Failure, Thread.Alerted } =
  CONST
    mask : Word.T = NUMBER(CHAR) - 1;
    b             = BITSIZE(CHAR);
  VAR
    c : CHAR;
  BEGIN
    c := VAL(Word.And(x, mask), CHAR);
    Wr.PutChar(wr, c);
    
    x := Word.RightShift(x, b);
    
    c := VAL(Word.And(x, mask), CHAR);
    Wr.PutChar(wr, c);
  END WriteUnsigned;

PROCEDURE ReadUnsigned(rd : Rd.T) : Unsigned
  RAISES { Rd.Failure, Thread.Alerted, Rd.EndOfFile } =
  VAR
    b0 := ORD(Rd.GetChar(rd));
    b1 := ORD(Rd.GetChar(rd));
  BEGIN
    RETURN Word.Or(Word.Shift(b1, 8), b0)
  END ReadUnsigned;

PROCEDURE ReadSigned(rd : Rd.T) : Signed 
  RAISES { Rd.Failure, Thread.Alerted, Rd.EndOfFile } =
  CONST
    sa = Word.Size - 8;
  VAR
    u := ReadUnsigned(rd);
  BEGIN
    (* sign extend u *)
    RETURN Word.RightShift(Word.Shift(u, sa), sa)
  END ReadSigned;
  
PROCEDURE WriteSigned(wr : Wr.T; xx : Signed)
  RAISES { Wr.Failure, Thread.Alerted } =
  CONST
    mask : Word.T = NUMBER(CHAR) - 1;
    b             = BITSIZE(CHAR);
  VAR
    c : CHAR;
    x : INTEGER := xx;
  BEGIN
    c := VAL(Word.And(x, mask), CHAR);
    Wr.PutChar(wr, c);
    
    x := Word.RightShift(x, b);
    
    c := VAL(Word.And(x, mask), CHAR);
    Wr.PutChar(wr, c);
  END WriteSigned;
  
(**********************************************************************)

PROCEDURE WriteT(wr : Wr.T; READONLY t : Rep16.T)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    WITH cw = Word.Or(Word.Shift(t.count, OrderBits),
                      t.order) DO
      WriteUnsigned(wr, cw)
    END;
    IF t.order = 0 THEN
      WriteUnsigned(wr, t.c0)
    ELSE
      FOR i := 1 TO t.order DO
        WriteSigned(wr, t.c[i])
      END
    END
  END WriteT;

PROCEDURE ReadT(rd : Rd.T; VAR t : Rep16.T) : CARDINAL
  RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  CONST
    oMask = Word.Shift(1, OrderBits) - 1;
  VAR
    w0 := ReadUnsigned(rd);
    bytes := 0;
  TYPE
    Count = Rep16.Count;
  BEGIN
    WITH cnt =  Word.RightShift(w0, OrderBits) DO
      IF cnt < FIRST(Count) OR cnt > LAST(Count) THEN
        Debug.Warning(F("Count wrong: w0 0x%s OrderBits %s cnt 0x%s FIRST 0x%s LAST 0x%s",
                        Fmt.Unsigned(w0), Int(OrderBits),
                        Fmt.Unsigned(cnt),
                        Fmt.Unsigned(FIRST(Count)), Fmt.Unsigned(LAST(Count))))
      END;
      t.count := cnt
    END;
    t.order := Word.And(w0, oMask);

    INC(bytes, 2);
    
    t.c := Zero;
    IF t.order = 0 THEN
      t.c0 := ReadUnsigned(rd);
      INC(bytes, 2)
    ELSE
      FOR i := 1 TO t.order DO
        t.c[i] := ReadSigned(rd);
        INC(bytes, 2)
      END
    END;
    RETURN bytes
  END ReadT;

(**********************************************************************)
  
PROCEDURE WriteHeader(wr : Wr.T; READONLY h : Rep16.Header)
  RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    a := ARRAY [0..1] OF LONGREAL { h.min, h.max };
  BEGIN
    UnsafeWriter.WriteI  (wr, h.nwords);
    UnsafeWriter.WriteI  (wr, h.npoints);
    UnsafeWriter.WriteLRA(wr, a)
  END WriteHeader;

PROCEDURE ReadHeader(rd : Rd.T; VAR h : Rep16.Header) : CARDINAL
  RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    a : ARRAY [0..1] OF LONGREAL;
  BEGIN
    h.nwords  := UnsafeReader.ReadI  (rd);
    h.npoints := UnsafeReader.ReadI  (rd);
    UnsafeReader.ReadLRA(rd, a);
    h.min := a[0];
    h.max := a[1];
    RETURN HeaderBytes
  END ReadHeader;
  
BEGIN END Rep16Stream.
