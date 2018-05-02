MODULE ServerPacket;
IMPORT ByteSeqRep AS Rep;
IMPORT ByteSeq;
IMPORT Wr, Thread, Rd;
IMPORT NetContext;
IMPORT Byte;

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
  VAR
    j : CARDINAL;
  BEGIN
    WITH n = NUMBER(t.elem^) DO
      FOR i := t.st TO t.st+t.sz-1 DO
        IF i >= n THEN
          j := i-n
        ELSE
          j := i
        END
      END;
      Wr.PutChar(wr, VAL(t.elem[j],CHAR)) (* could use UnsafeWr here *)
    END
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

BEGIN END ServerPacket.
