MODULE ArithCode;
IMPORT ArithCoder AS Coder;
IMPORT FreqTable;
IMPORT ArithCallback AS Callback;
IMPORT Text;
IMPORT ArithBits AS Bits;
IMPORT ArithProbability AS Probability;
IMPORT Word;
IMPORT Fmt; FROM Fmt IMPORT FN, Unsigned, F, Int, LongReal;
IMPORT Debug;

CONST LR = LongReal;

REVEAL
  T = Public BRANDED Brand OBJECT
    freqs : FreqTable.T;
  OVERRIDES
    init       := Init;
    newEncoder := NewEncoder;
    newDecoder := NewDecoder;
  END;

  Coder.T = Coder.Public BRANDED Coder.Brand OBJECT
    t      : T;
    cb     : Callback.T;
    cum    : FreqTable.Cum;
    lo, hi : CodeValue;
    bits   : CARDINAL;
    bytes  : CARDINAL;
  METHODS
    init(t : T) : Coder.T := InitCoder;
    getProbability(c : EncodeType) : Probability.T := GetProbability;
  OVERRIDES
    chars       := CoderChars;
    text        := CoderText;
    setCallback := SetCallback;
  END;

  Callback.T = Callback.Public BRANDED Callback.Brand OBJECT
    ecb : Callback.T;
  OVERRIDES
    setErrorCb := SetErrorCb;
  END;

PROCEDURE Init(t : T; READONLY freqs : FreqTable.T) : T =
  BEGIN
    t.freqs := freqs;
    RETURN t
  END Init;

PROCEDURE InitCoder(coder : Coder.T; t : T) : Coder.T =
  BEGIN
    coder.t := t;
    FreqTable.Accumulate(t.freqs, coder.cum);
    coder.lo := 0;
    coder.hi := Bits.MaxCode;
    RETURN coder
  END InitCoder;
  
PROCEDURE NewEncoder(t : T) : Coder.T =
  BEGIN
    WITH res = NEW(Encoder).init(t) DO
      RETURN res
    END
  END NewEncoder;

PROCEDURE NewDecoder(t : T) : Coder.T =
  BEGIN
    WITH res = NEW(Decoder).init(t) DO
      RETURN res
    END
  END NewDecoder;

PROCEDURE SetErrorCb(cb : Callback.T; ecb : Callback.T) =
  BEGIN
    cb.ecb := ecb
  END SetErrorCb;
  
  (**********************************************************************)
  (*                                                                    *)
  (*                        GENERIC  METHODS                            *)
  (*                                                                    *)
  (**********************************************************************)

PROCEDURE SetCallback(en : Coder.T; cb : Callback.T) =
  BEGIN
    en.cb := cb;
  END SetCallback;

PROCEDURE CoderChars(c : Coder.T; READONLY a : ARRAY OF CHAR) =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      c.char(a[i])
    END
  END CoderChars;

PROCEDURE CoderText(c : Coder.T; txt : TEXT) =
  BEGIN
    FOR i := 0 TO Text.Length(txt) - 1 DO
      c.char(Text.GetChar(txt, i))
    END
  END CoderText;
  
  (**********************************************************************)
  (*                                                                    *)
  (*                    CODEC SHARED DEFINITIONS                        *)
  (*                                                                    *)
  (**********************************************************************)

TYPE
  CodeValue = Bits.T;

PROCEDURE GetProbability(coder : Coder.T; c : EncodeType) : Probability.T =
  BEGIN
    
    WITH res = Probability.T { lo    := coder.cum[c],
                               hi    := coder.cum[c + 1],
                               count := coder.cum[LAST(coder.cum)] } DO
      <*ASSERT res.lo <= res.hi*>
      <*ASSERT res.count # 0*>
      <*ASSERT res.hi # 0*> (* cant be called like this *)
      RETURN res
    END
  END GetProbability;
  
  (**********************************************************************)
  (*                                                                    *)
  (*                            ENCODER                                 *)
  (*                                                                    *)
  (**********************************************************************)

TYPE
  Encoder = Coder.T BRANDED OBJECT
    pending_bits : CARDINAL;

    pendingEncodedByte  : [0..255];
    nextBit             : [0..7];
  METHODS
    encode(c : EncodeType)              := Encode;
    putBitPlusPending(bit : [0..1])     := PutBitPlusPending;
    flushFinalByteAndEof()              := FlushFinalByteAndEof;
  OVERRIDES
    char := EnChar;
    eof  := EnEof;
  END;

  EncodeType = [0..ORD(LAST(CHAR)) + 1];

PROCEDURE PutBitPlusPending(en : Encoder; bit : [0..1]) =

  PROCEDURE Put(b : [0..1]) =
    BEGIN
      en.pendingEncodedByte := Word.Or(en.pendingEncodedByte,
                                       Word.Shift(b, en.nextBit));
      IF en.nextBit = 7 THEN
        en.cb.newByte(VAL(en.pendingEncodedByte,CHAR));
        en.nextBit := 0;
        en.pendingEncodedByte := 0;
      ELSE
        INC(en.nextBit)
      END
    END Put;
    
  BEGIN
    Put(bit);
    FOR i := 0 TO en.pending_bits - 1 DO
      Put(1 - bit)
    END;
    en.pending_bits := 0
  END PutBitPlusPending;

PROCEDURE FlushFinalByteAndEof(en : Encoder) =
  BEGIN
    (* this dumps a few higher-order bits as zero but they won't be decoded by
       the decoder *)
    IF en.nextBit # 0 THEN
      en.cb.newByte(VAL(en.pendingEncodedByte, CHAR));
      en.nextBit := 0;
      en.pendingEncodedByte := 0;
    END;
    en.cb.newEof()
  END FlushFinalByteAndEof;

CONST Verbose = TRUE;

PROCEDURE FmtET(c : EncodeType) : TEXT =
  BEGIN
    IF c = LAST(EncodeType) THEN
      RETURN "<*EOF*>"
    ELSE
      RETURN Text.FromChar(VAL(c, CHAR))
    END
  END FmtET;

TYPE AT = ARRAY OF TEXT;
  
PROCEDURE Encode(en : Encoder; c : EncodeType) =
  BEGIN
    <*ASSERT Bits.GE(en.hi, en.lo)*>
    
    WITH p     = en.getProbability(c),
         range = en.hi - en.lo + 1,
         newHi = Bits.Plus(en.lo,
                           Bits.Minus(Bits.Divide(Bits.Times(range, p.hi), p.count),
                                      1)),
         newLo = Bits.Plus(en.lo,
                           Bits.Minus(Bits.Divide(Bits.Times(range, p.lo), p.count),
                                      1)) DO
      (* some assertions go here, eh? *)
      IF Verbose THEN
        Debug.Out(FN("Encoder(%s) : en.hi = %s en.lo = %s; range %s; p %s; newHi %s newLo %s ",
                     AT { FmtET(c),
                          Unsigned(en.hi), Unsigned(en.lo),
                          Unsigned(range), Probability.Format(p),
                          Unsigned(newHi), Unsigned(newLo) }))
      END;
      
      en.hi := newHi;
      en.lo := newLo;
    END;

    LOOP
      IF    Bits.LT(en.hi, Bits.OneHalf) THEN
        IF Verbose THEN
          Debug.Out("putBitPlusPending 0 (en.hi < 1/2)")
        END;
        en.putBitPlusPending(0);
        INC(en.bits)
      ELSIF Bits.GE(en.lo, Bits.OneHalf) THEN
        IF Verbose THEN
          Debug.Out("putBitPlusPending 1 (en.lo > 1/2)")
        END;
        en.putBitPlusPending(1);
        INC(en.bits)
      ELSIF Bits.GE(en.lo, Bits.OneFourth) AND Bits.LT(en.hi, Bits.ThreeFourths) THEN
        INC(en.pending_bits);
        INC(en.bits);

        IF Verbose THEN
          Debug.Out(F("INC(pendingBits->%s)    (en.lo >= 1/4 && en.hi < 3/4)",
                      Int(en.pending_bits)))
        END;

        WITH fixLo = Bits.Minus(en.lo, Bits.OneFourth),
             fixHi = Bits.Minus(en.hi, Bits.OneFourth) DO

          (* some assertions go here? *)
          en.hi := fixHi;
          en.lo := fixLo
        END
      ELSE
        EXIT
      END;

      WITH shftHi = Bits.Plus(Bits.Shift(en.hi, 1), 1),
           shftLo =           Bits.Shift(en.lo, 1),
           maskHi = Bits.And(shftHi, Bits.MaxCode),
           maskLo = Bits.And(shftLo, Bits.MaxCode) DO
        en.hi := maskHi;
        en.lo := maskLo
      END;
    END;

    IF c = LAST(EncodeType) THEN
      (* what to do here? *)
      INC(en.pending_bits);
      INC(en.bits);
      IF Bits.LT(en.lo, Bits.OneFourth) THEN
        en.putBitPlusPending(0);
        INC(en.bits)
      ELSE
        en.putBitPlusPending(1);
        INC(en.bits)
      END;

      en.flushFinalByteAndEof();
      
    END;
    INC(en.bytes);
    IF Verbose THEN
      WITH bpb = FLOAT(en.bits, LONGREAL) / FLOAT(en.bytes, LONGREAL),
           eff = 100.0d0 / 8.0d0 * bpb DO
        
      Debug.Out(F("encoded %s bytes -> %s bits (%s b.p.b. = %s%)",
                  Int(en.bytes), Int(en.bits), LR(bpb), LR(eff)))
      END
    END
  END Encode;

PROCEDURE EnChar(en : Encoder; c : CHAR) =
  BEGIN
    en.encode(ORD(c))
  END EnChar;

PROCEDURE EnEof(en : Encoder) =
  BEGIN
    en.encode(ORD(LAST(CHAR)) + 1)
  END EnEof;


  (**********************************************************************)
  (*                                                                    *)
  (*                            DECODER                                 *)
  (*                                                                    *)
  (**********************************************************************)

TYPE
  Decoder = Coder.T BRANDED OBJECT 
  OVERRIDES
  END;


BEGIN END ArithCode.
