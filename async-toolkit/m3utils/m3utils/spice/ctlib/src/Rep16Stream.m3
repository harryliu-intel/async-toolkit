MODULE Rep16Stream;
IMPORT Rep16;
IMPORT Rd, Wr;
IMPORT UnsafeReader, UnsafeWriter;
IMPORT Thread;
IMPORT Word;
FROM Rep16 IMPORT Signed, Unsigned, OrderBits, Zero;
IMPORT Fmt; FROM Fmt IMPORT Int, F, Pad;
IMPORT Debug;
IMPORT TextWr;
IMPORT TextRd;
IMPORT Text;

PROCEDURE Bytes(t : Rep16.T) : CARDINAL =
  BEGIN
    IF t.order = 0 THEN
      RETURN (1 + 1) * 2
    ELSIF t.reset THEN
      RETURN (2 + t.order) * 2
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
  VAR
    u := ReadUnsigned(rd);
    res : INTEGER := u;
  BEGIN
    IF res >= Word.Shift(1, 15) THEN
      DEC(res, Word.Shift(1, 16))
    END;
    RETURN res
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
    WITH resetI     = ARRAY BOOLEAN OF INTEGER { 0, 1 }[t.reset],
         countInPos = Word.Shift(t.count, OrderBits + 1),
         resetInPos = Word.Shift(resetI, OrderBits),
         orderInPos = t.order,

         cw = Word.Or(countInPos, Word.Or(resetInPos, orderInPos)) DO
      WriteUnsigned(wr, cw)
    END;
    IF t.order = 0 OR t.reset THEN
      WriteUnsigned(wr, t.c0)
    END;
    FOR i := 1 TO t.order DO
      WriteSigned(wr, t.c[i])
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
    WITH reset = Word.And(Word.RightShift(w0, OrderBits),1) # 0 DO
      t.reset := reset
    END;
    
    WITH cnt =  Word.RightShift(w0, OrderBits + 1) DO
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
    IF t.order = 0 OR t.reset THEN
      t.c0 := ReadUnsigned(rd);
      INC(bytes, 2)
    END;
    
    FOR i := 1 TO t.order DO
      t.c[i] := ReadSigned(rd);
      INC(bytes, 2)
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

PROCEDURE DebugTxt(txt : TEXT) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := 0 TO Text.Length(txt) - 1 DO
      res := res & Pad( Fmt.Unsigned(ORD(Text.GetChar(txt, i))),
                                     2,
                                     padChar := '0');
      res := res & " "
    END;
    RETURN res
  END DebugTxt;
  
PROCEDURE DoTest() =
  <*FATAL Thread.Alerted, Wr.Failure, Rd.Failure, Rd.EndOfFile*>
  BEGIN
    FOR i := FIRST(Unsigned) TO LAST(Unsigned) DO
      WITH wr = TextWr.New() DO
        WriteUnsigned(wr, i);
        WITH txt = TextWr.ToText(wr),
             rd  = TextRd.New(txt),
             rb  = ReadUnsigned(rd) DO
          IF rb # i THEN
            Debug.Error(F("Unsigned r/w verify error for %s (wrote %s), read %s",
                          Int(i), DebugTxt(txt), Int(rb)))
          END
        END
      END
    END;
    FOR i := FIRST(Signed) TO LAST(Signed) DO
      WITH wr = TextWr.New() DO
        WriteSigned(wr, i);
        WITH txt = TextWr.ToText(wr),
             rd  = TextRd.New(txt),
             rb  = ReadSigned(rd) DO
          IF rb # i THEN
            Debug.Error(F("Signed r/w verify error for %s (wrote %s), read %s",
                          Int(i), DebugTxt(txt), Int(rb)))
          END
        END
      END
    END
  END DoTest;

CONST Verify = TRUE;
      
BEGIN
  IF Verify THEN DoTest() END
END Rep16Stream.
