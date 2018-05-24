MODULE ServerPacket;
IMPORT ByteSeqRep AS Rep;
IMPORT ByteSeq;
IMPORT Wr, Thread, Rd;
IMPORT NetContext;
IMPORT Byte;
IMPORT Word;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F;

TYPE
  Super = ByteSeq.T;
  
REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    prepPfx := PrepPfx;
    init := Init;
  END;

PROCEDURE PrepPfx(t : T; pfxSz : CARDINAL) =
  BEGIN
    WITH newSz = t.sz + pfxSz DO
      WHILE t.sz < newSz DO Expand(t) END;
      IF t.st > pfxSz THEN
        DEC(t.st,pfxSz)
      ELSE
        t.st := t.st + NUMBER(t.elem^) - pfxSz
      END
    END
  END PrepPfx;
  
PROCEDURE Expand(s: T) = (* c-n-p from Sequence.mg :-( *)
  VAR 
    n := NUMBER(s.elem^);
    new := NEW(Rep.RefArray, 2 * n);
    m := n - s.st;
  BEGIN
    SUBARRAY(new^, 0, m) := SUBARRAY(s.elem^, s.st, m);
    SUBARRAY(new^, m, s.st) :=
      SUBARRAY(s.elem^, 0, s.st);
    s.st := 0;
    s.elem := new
  END Expand;

PROCEDURE Put(t : T; i : CARDINAL; c : Byte.T) =
  BEGIN
    WHILE i >= t.size() DO t.addhi(VAL(0,Byte.T)) END; (* ugly *)
    ByteSeq.T.put(t,i,c)
  END Put;

PROCEDURE Get(t : T; i : CARDINAL) : Byte.T =
  BEGIN RETURN ByteSeq.T.get(t,i) END Get;

PROCEDURE Transmit(t :T; wr : Wr.T) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    WITH n = NUMBER(t.elem^) DO
      FOR i := t.st TO t.st+t.sz-1 DO
        VAR
          j : CARDINAL;
        BEGIN
          IF i >= n THEN
            j := i-n
          ELSE
            j := i
          END;
          Wr.PutChar(wr, VAL(t.elem[j],CHAR)) (* could use UnsafeWr here *)
        END
      END
    END;
    Wr.Flush(wr)
  END Transmit;

PROCEDURE Init(s: T; sizeHint: CARDINAL): Super = 
  BEGIN
    IF s.elem = NIL OR NUMBER(s.elem^) = 0 THEN
      s.elem := NEW(Rep.RefArray, MAX(sizeHint, 1))
    ELSE
      (* no need to clear the previous entries to help the GC for Byte.T *)
    END (* IF *);
    s.sz := 0; s.st := 0;
    RETURN s
  END Init;

PROCEDURE PutE(t : T; e : End; c : Byte.T) =
  BEGIN
    CASE e OF
      End.Front => ByteSeq.T.addlo(t,c)
    |
      End.Back  => ByteSeq.T.addhi(t,c)
    END
  END PutE;

PROCEDURE FromRd(t : T; rd : Rd.T; VAR cx : NetContext.T) : T
  RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  BEGIN
    FOR i := 0 TO cx.rem-1 DO
      t.addhi(ORD(Rd.GetChar(rd)))
    END;
    cx.rem := 0;
    RETURN t
  END FromRd;

PROCEDURE ExtractBits(t                      : T;
                      byteOffset, startBit   : CARDINAL;
                      numBits                : [0..BITSIZE(Word.T)];
                      VAR w                  : Word.T) : BOOLEAN =
  VAR
    loByte := byteOffset +  startBit DIV 8;
    loBit  :=               startBit MOD 8;
    hiByte := byteOffset + (startBit + numBits - 1) DIV 8;
    (* byte holding last bit *)
    
    b := 0;
  BEGIN
    IF hiByte > t.size()-1 THEN RETURN FALSE END;
    w := 0;
    FOR i := loByte TO hiByte DO
      w := Word.Insert(w, Word.RightShift(t.get(i), loBit), b, 8-loBit);
      INC(b, 8-loBit);
      loBit := 0;
    END;
    w := Word.Insert(w, 0, numBits, BITSIZE(Word.T)-numBits);
    RETURN TRUE
  END ExtractBits;

PROCEDURE ArrPut(VAR a    : ARRAY OF Byte.T;
                 startBit : CARDINAL;
                 w        : Word.T;
                 numBits  : CARDINAL) =
  VAR
    loByte := startBit DIV 8;
    loBit  := startBit MOD 8;
    hiByte := (startBit + numBits - 1) DIV 8;
    (* byte holding last bit *)
    
    b := 0;
  BEGIN
    (* clear top bits of w *)
    WITH blankbits = BITSIZE(Word.T)-numBits DO
      w := Word.RightShift(Word.LeftShift(w, blankbits), blankbits)
    END;
    FOR i := loByte TO hiByte DO
      a[i] := Word.Insert(a[i], Word.RightShift(w, b), loBit, 8-loBit);
      IF FALSE THEN
        Debug.Out(F("w=16_%s bits=%s:+%s a[%s]=16_%s",
                    Fmt.Unsigned(w),
                    Fmt.Int(startBit),
                    Fmt.Int(numBits),
                    Fmt.Int(i),
                    Fmt.Unsigned(a[i])))
      END;
      INC(b, 8-loBit);
      loBit := 0;
    END
  END ArrPut;

PROCEDURE PutA(t : T; e : End; READONLY a : ARRAY OF Byte.T) =
  BEGIN
     CASE e OF
       End.Front =>
       FOR i := LAST(a) TO FIRST(a) BY -1 DO
         ByteSeq.T.addlo(t,a[i])
       END
    |
      End.Back  =>
      FOR i := FIRST(a) TO LAST(a) BY 1 DO
        ByteSeq.T.addhi(t,a[i])
      END
    END
  END PutA;

PROCEDURE PutWLE(t : T; e : End; w : Word.T; bytes : [0..BYTESIZE(Word.T)]) =
  VAR
    arr : ARRAY [0..BYTESIZE(Word.T)-1] OF Byte.T;
  BEGIN
    FOR i := 0 TO BYTESIZE(Word.T)-1 DO
      arr[i] := Word.Extract(w, 8*i,8)
    END;
    PutA(t, e, SUBARRAY(arr, 0, bytes))
  END PutWLE;

PROCEDURE Truncate(t : T; e : End; by : CARDINAL) =
  BEGIN
    <*ASSERT by <= t.sz*>
    DEC(t.sz,by);
    CASE e OF
      End.Front =>
      INC(t.st, by);
      IF t.st > NUMBER(t.elem^) THEN DEC(t.st,NUMBER(t.elem^)) END
    |
      End.Back => (* skip *)
    END
  END Truncate;

  
BEGIN END ServerPacket.
