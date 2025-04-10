(* $Id: BigInt.m3,v 1.9 2003/08/21 03:28:30 kp Exp $ *)

MODULE BigInt;
IMPORT Integer;
IMPORT CharSeq;
IMPORT Word;
IMPORT Wx;
IMPORT Text;
IMPORT Lex, FloatMode;
IMPORT Scan AS M3Scan;
IMPORT BigIntBigIntTbl;
IMPORT Debug;
FROM Fmt IMPORT Int, F;

CONST BaseLog2 =   BITSIZE(Word.T) DIV 2 - 1; (* (*was*) 10 *) 
      Base     =  Word.Shift(1, BaseLog2); 
      (* must be less than or equal to sqrt(LAST(CARDINAL)) *)
      (* must be power of 2 *)

(********************* sequence of cardinals *********************)

(* other sequence impl *)

CONST doDebug = FALSE;
      
TYPE 
  NArry = REF ARRAY OF CARDINAL;

  NSeq = RECORD
    siz : CARDINAL; (* # of significant digits *)
    a   : NArry;
  END;

  (*
  METHODS
    init(hintSize : CARDINAL := 5) : NSeq := InitN;
    shift(sa : CARDINAL) := ShiftN;
    extend(toBits : CARDINAL) := ExtendN;
    clearTop() := ClearTop;
    copy() : NSeq := CopyN;
    size() : CARDINAL := SizeN;
  END;
  *)

PROCEDURE InitN(VAR s : NSeq; hintSize : CARDINAL) =
  BEGIN
    s.siz := 0;
    s.a := NEW(NArry, hintSize);
    FOR i := FIRST(s.a^) TO LAST(s.a^) DO s.a[i] := 0 END
  END InitN;

PROCEDURE ShiftN(VAR s : NSeq; sa : CARDINAL) =
  VAR
    os := s.siz;
  BEGIN
    INC(s.siz,sa);
    ExtendN(s, s.siz);
    SUBARRAY(s.a^,sa,os) := SUBARRAY(s.a^,0,os);
    FOR i := 0 TO sa - 1 DO s.a[i] := 0 END;
  END ShiftN;

PROCEDURE ExtendN(VAR s : NSeq; toDigits : CARDINAL) =
  BEGIN
    IF NUMBER(s.a^) < toDigits THEN
      VAR 
        na := NEW(NArry,toDigits);
      BEGIN
        SUBARRAY(na^,0,NUMBER(s.a^)) := s.a^;

        FOR i := NUMBER(s.a^) TO LAST(na^) DO na[i] := 0 END;

        s.a := na
      END
    END
  END ExtendN;

PROCEDURE CopyN(READONLY s : NSeq) : NSeq =
  VAR
    na := NEW(NArry, NUMBER(s.a^));
  BEGIN 
    na^ := s.a^;
    RETURN NSeq { siz := s.siz, a := na }
  END CopyN;

PROCEDURE ClearTop(VAR s : NSeq) = 
  BEGIN
    FOR i := LAST(s.a^) TO 0 BY -1 DO
      IF s.a[i] = 0 THEN s.siz := i ELSE EXIT END
    END;
  END ClearTop;

(********************* bignum type *********************)

REVEAL 
  T = Public BRANDED Brand OBJECT
    sign : [ -1 .. 1 ];
    rep  : NSeq;
  END;
  
PROCEDURE Compare(a, b : T) : CompRet =
  BEGIN
    IF a.sign < b.sign THEN RETURN -1
    ELSIF a.sign > b.sign THEN RETURN 1
    END;

    IF    a.rep.siz > b.rep.siz THEN RETURN  a.sign 
    ELSIF a.rep.siz < b.rep.siz THEN RETURN -a.sign
    END;

    FOR i := a.rep.siz - 1 TO 0 BY -1 DO
      VAR
        c := Integer.Compare(a.rep.a[i],b.rep.a[i]);
      BEGIN
        IF c # 0 THEN RETURN a.sign * c END
      END
    END;

    RETURN 0
  END Compare;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN Compare(a,b) = 0 END Equal;

PROCEDURE IsZero(a : T) : BOOLEAN = BEGIN RETURN Compare(a, Zero) = 0 END IsZero;

PROCEDURE Copy(t : T) : T =
  (* Uniq makes no sense here really... *)
  BEGIN RETURN t END Copy;

PROCEDURE New(x : INTEGER) : T =
  VAR
    c : CARDINAL;
    s : NSeq;
    corr : [-1..0] := 0;
    res : T;
  BEGIN
    InitN(s, 1);
    IF doDebug THEN Debug.Out("New x : " & Int(x)) END;
    
    IF x = FIRST(INTEGER) THEN
      (* very special case! can't represent ABS(FIRST(INTEGER)) as a CARDINAL *)
      corr := -1; (* remember correction for later *)
      x := x + 1; (* bring it in range *)
    END;
    
    c := ABS(x);
    s.a[0] := c;
    s.siz := 1;

    Renormalize(s);

    IF x >= 0 THEN
      res := NEW(T, sign := +1, rep := s)
    ELSE
      res := NEW(T, sign := -1, rep := s)
    END;

    IF corr = -1 THEN (* fix up special case of FIRST(INTEGER) *)
      RETURN Uniq(Sub(res, One))
    ELSE
      RETURN Uniq(res)
    END
  END New;

PROCEDURE Sign(a : T) : CompRet =
  BEGIN
    IF a.rep.siz = 0 THEN RETURN 0; END;
    RETURN a.sign;
  END Sign;

PROCEDURE Divide(a, b : T; VAR q, r : T) = 
  (* first do a C-style divide *)
  VAR
    sign := a.sign * b.sign;
    aa := Abs(a);
    bb := Abs(b);
  BEGIN
    DivideUnsigned(aa,bb,q,r);
    IF sign < 0 THEN q := Neg(q); r := Neg(r) END;

    (* if the remainder is not 0 we need to fix up the result *)
    IF r.rep.siz # 0 THEN

      (* r in [0,b) *)
      IF sign # b.sign THEN r := Add(r, b); END;

      (* a-r=b*q *)
      IF sign = -1 THEN q := Sub(q, One); END;

    END;
  END Divide;

PROCEDURE Div(a, b : T) : T =
  VAR
    q, r : T;
  BEGIN
    Divide(a,b,q,r);
    RETURN q
  END Div;

PROCEDURE Mod(a, b : T) : T =
  VAR
    q, r : T;
  BEGIN
    Divide(a,b,q,r);
    RETURN r
  END Mod;
    
PROCEDURE Shift(a : T; sa : CARDINAL) : T =
  VAR
    seq := CopyN(a.rep);
  BEGIN
    <*ASSERT Zero = Uniq(Zero)*>
    ShiftN(seq, sa);
    <*ASSERT Zero = Uniq(Zero)*>

    WITH res = Uniq(NEW(T, sign := a.sign, rep := seq)) DO
          <*ASSERT Zero = Uniq(Zero)*>
      RETURN res
    END
  END Shift;

PROCEDURE DebugSeq(READONLY seq : NSeq) : TEXT =
  VAR
    res := F("{ siz=%s; ", Int(seq.siz));
  BEGIN
    FOR i := NUMBER(seq.a^) - 1 TO 0 BY -1 DO
      res := res & Int(seq.a[i]) & " "
    END;
    RETURN res & "}"
  END DebugSeq;

PROCEDURE DebugT(t : T) : TEXT =
  BEGIN
    RETURN F("[ sgn %s %s]", Int(t.sign), DebugSeq(t.rep))
  END DebugT;
  
PROCEDURE DivideUnsigned(aparm, b : T; VAR q, r : T) =
  VAR
    s : T;
    lo, hi : CARDINAL;
  BEGIN
    IF doDebug THEN Debug.Out(F("DivideUnsigned aparm  = " & DebugT(aparm))) END;
    IF doDebug THEN Debug.Out(F("DivideUnsigned b      = " & DebugT(b))) END;

    q := Zero;
    r := aparm;
    <* ASSERT aparm.sign = 1 AND b.sign = 1 *>

    FOR sa := aparm.rep.siz - b.rep.siz TO 0 BY -1 DO
      s := Shift(b,sa);

      IF doDebug THEN Debug.Out(F("sa = %s ; s = %s", Int(sa), DebugT(s))) END;
      
      (* search for digit ... *)
      lo := 0;
      hi := Base;

      WHILE lo < hi - 1 DO
        VAR
          mid  := (lo + hi) DIV 2;

          (*
            loM  := Mul(New(lo),s);
            hiM  := Mul(New(hi),s);
          *)

          midM := Mul(New(mid),s);
          midC := Compare(midM,r);

        BEGIN

          IF doDebug THEN Debug.Out(F("mid = %s, midM = %s, midC = %s",
                      Int(mid),
                      DebugT(midM),
                      Int(midC))) END;

          (*<* ASSERT Compare(loM,r) <= 0 AND Compare(hiM,r) >= 0 *>*)
          CASE midC OF
            -1 => lo := mid
          |
            0 =>
            q := Add(Shift(New(mid),sa),q);
            r := Zero;
            <* ASSERT Equal(aparm, Add(r, Mul(q,b))) *>
            RETURN
          |
             1 => hi := mid
          END
        END;
      END;

      WITH nl = New(lo) DO
        WITH z = Shift(nl, sa) DO
          IF doDebug THEN Debug.Out(F("lo = %s ; sa = %s; nl = %s; z = %s, q = %s",
                      Int(lo), Int(sa), DebugT(nl), DebugT(z), DebugT(q)))END;
          q := Add(z,q);
        END
      END;
      r := Sub(r, Mul(New(lo),s));
      IF doDebug THEN Debug.Out(F("q = %s ; r = %s", DebugT(q), DebugT(r))) END;
    END;
    <* ASSERT Equal(aparm, Add(r, Mul(q,b))) *>
  END DivideUnsigned;

PROCEDURE Mul(a, b : T) : T =
  BEGIN
    RETURN Uniq(NEW(T, sign := a.sign * b.sign, rep := MulSeqs(a.rep,b.rep)))
  END Mul;

PROCEDURE Pow(b, x : T) : T =
  VAR
    r : T;
    result := One;
  BEGIN
    IF x.sign = -1 THEN RETURN Zero END;

    <* ASSERT x.sign = 1 *>

    WHILE NOT IsZero(x) DO
      Divide(x, Two, x, r);
      IF NOT IsZero(r) THEN
        result := Mul(result, b)
      END;
      b := Mul(b, b)
    END;
    RETURN result
  END Pow;

PROCEDURE MulSeqs(READONLY a, b : NSeq) : NSeq =
  VAR 
    res : NSeq;
    idx : CARDINAL;
    s : CARDINAL;
  BEGIN
    InitN(res, a.siz + b.siz + 1 );
    FOR i := 0 TO a.siz - 1 DO
      FOR j := 0 TO b.siz - 1 DO
        idx := i+j;
        s := a.a[i] * b.a[j];
        WHILE s # 0 DO
          VAR
            o := res.a[idx];
          BEGIN
            s := s + o;
            res.a[idx] := s MOD Base;
            s := s DIV Base;
            INC(idx)
          END
        END
      END
    END;
    Renormalize(res);
    RETURN res
  END MulSeqs;

PROCEDURE Add(a, b : T) : T =
  VAR
    res : T;
  BEGIN
    IF doDebug THEN Debug.Out(F("Add( a = %s , b = %s )", DebugT(a), DebugT(b))) END;
    
   IF a.sign = 1 AND b.sign = 1 THEN
      res := NEW(T, sign := 1, rep := AddSeqs(a.rep,b.rep));
    ELSIF a.sign = -1 AND b.sign = -1 THEN
      res := NEW(T, sign := -1, rep := AddSeqs(a.rep,b.rep));
    ELSIF a.sign = -1 AND b.sign = 1 THEN
      res := Sub(b,Neg(a));
    ELSIF a.sign = 1 AND b.sign = -1 THEN
      res := Sub(a,Neg(b));
    ELSE
      <* ASSERT FALSE *>
    END;

    Renormalize(res.rep);

    IF doDebug THEN Debug.Out(F("Add( a = %s , b = %s ) ->  %s", DebugT(a), DebugT(b), DebugT(res))) END;
    RETURN Uniq(res)
  END Add;

PROCEDURE Sub(a, b : T) : T =
  VAR
    res : T;
  BEGIN
    IF a.sign = 1 AND b.sign = 1 AND Compare(a,b) > -1 THEN
      res := NEW(T, sign := 1, rep := SubSeqs(a.rep,b.rep))
    ELSIF a.sign = 1 AND b.sign = 1 AND Compare(a,b) = -1 THEN
      res := NEW(T, sign := -1, rep := SubSeqs(b.rep,a.rep))
    ELSIF a.sign = -1 AND b.sign = -1 THEN
      res := Neg(Sub(Neg(a),Neg(b)))
    ELSIF a.sign = -1 AND b.sign = 1 THEN
      res := Neg(Add(Neg(a),b))
    ELSIF a.sign = 1 AND b.sign = -1 THEN
      res := Add(a,b)
    ELSE
      <* ASSERT FALSE *>
    END;
    
    Renormalize(res.rep);
    RETURN Uniq(res)
  END Sub;

(* unsigned addition of underlying sequences *)
PROCEDURE AddSeqs(READONLY s, t : NSeq) : NSeq =
  VAR
    m := MAX(s.siz, t.siz);
    r : NSeq;
  BEGIN
    InitN(r, m + 1);
    SUBARRAY(r.a^, 0, s.siz) := SUBARRAY(s.a^, 0, s.siz);
    FOR i := 0 TO MIN(m - 1, t.siz - 1) DO
      INC(r.a[i], t.a[i]);
    END;
    RETURN r
  END AddSeqs;

(* unsigned subtraction of underlying sequences *)
(* s must be .ge. t *)
PROCEDURE SubSeqs(READONLY s, t : NSeq) : NSeq =
  VAR
    m := MAX(s.siz, t.siz);
    r : NSeq;
    borrow := 0;
  BEGIN
    <*ASSERT s.siz >= t.siz*>

    InitN(r, m + 1);
    FOR i := 0 TO m - 1 DO
      VAR
        diff := s.a[i] + borrow;
      BEGIN
        IF i < t.siz THEN DEC(diff, t.a[i]); END;
        borrow := 0;

        WHILE diff < 0 DO
          diff := diff + Base;
          borrow := borrow - 1
        END;
        r.a[i] := diff
      END
    END;
    <* ASSERT borrow = 0 *>
    RETURN r
  END SubSeqs;

PROCEDURE Abs(a : T) : T = 
  BEGIN RETURN Uniq(NEW(T, sign := 1, rep := a.rep)) END Abs;

PROCEDURE Renormalize(VAR a : NSeq) = 
  VAR
    carry := 0;
    o : CARDINAL;
    i := 0;
  BEGIN
    ClearTop(a);

    WHILE i < a.siz OR carry # 0 DO
      
      IF i > LAST(a.a^) THEN ExtendN(a, NUMBER(a.a^) + 4) END;

      <*ASSERT i <= LAST(a.a^)*>
      
      o := a.a[i] + carry;
      a.a[i] := o MOD Base;

      IF i >= a.siz AND a.a[i] # 0 THEN a.siz := i + 1 END;

      carry := o DIV Base;
      INC(i)
    END;
  END Renormalize;

PROCEDURE Neg(a : T) : T = 
  BEGIN RETURN Uniq(NEW(T, rep := a.rep, sign := -a.sign)) END Neg;

PROCEDURE ScanBased(txt : TEXT; defaultBase : PrintBase) : T
  RAISES { Lex.Error, FloatMode.Trap } =
  VAR
    neg : BOOLEAN;
  BEGIN
    <*ASSERT Zero=Uniq(Zero)*>
    IF Text.GetChar(txt, 0) = '-' THEN
      neg := TRUE;
      txt := Text.Sub(txt, 1, Text.Length(txt) - 1)
    ELSE
      neg := FALSE
    END;

    (* "" and "-" are not legal numbers *)
    IF Text.Length(txt) = 0 THEN
      RAISE Lex.Error
    END;

    <*ASSERT Zero=Uniq(Zero)*>
    
    WITH usIndex = Text.FindChar(txt, '_') DO
      IF usIndex = -1 THEN
        RETURN Scan(txt, defaultBase, neg)
      ELSE
        WITH baseTxt = Text.Sub(txt, 0, usIndex),
             base    = M3Scan.Int(baseTxt),
             mantTxt = Text.Sub(txt, usIndex + 1) DO
          RETURN Scan(mantTxt, base, neg)
        END
      END
    END
  END ScanBased;
  
PROCEDURE Scan(txt : TEXT; base : PrintBase; neg : BOOLEAN) : T
  RAISES { Lex.Error } =
  VAR
    accum := Zero;
    baseT := small[base];
  BEGIN
    IF Text.GetChar(txt, 0) = '-' THEN
      neg := NOT neg;
      txt := Text.Sub(txt, 1, Text.Length(txt) - 1)
    END;

    <*ASSERT Zero=Uniq(Zero)*>

    (* "" and "-" are not legal numbers *)
    IF Text.Length(txt) = 0 THEN
      RAISE Lex.Error
    END;
     
    FOR i := 0 TO Text.Length(txt) - 1 DO
      WITH c = Text.GetChar(txt, i) DO
          WITH val = CharVal[c] DO
            IF val < 0 OR val > base - 1 THEN
              RAISE Lex.Error
            END;
            accum := Add(Mul(baseT, accum), CharValT[c])
          END
      END
    END;

    <*ASSERT Zero=Uniq(Zero)*>

    IF neg THEN
      RETURN Neg(accum)
    ELSE
      RETURN accum
    END
  END Scan;

PROCEDURE ScanDelimited(txt : TEXT; base : PrintBase) : T
  RAISES { Lex.Error } =
  VAR
    accum := Zero;
    baseT := small[base];
  BEGIN
    <* ASSERT base <= NUMBER(HexChars) *>
    FOR i := 0 TO Text.Length(txt) - 1 DO
      WITH c = Text.GetChar(txt, i) DO
        IF c IN DelimChars THEN
          (* skip *)
        ELSE
          WITH val = CharVal[c] DO
            IF val < 0 OR val > base - 1 THEN
              RAISE Lex.Error
            END;
            accum := Add(Mul(baseT, accum), CharValT[c])
          END
        END
      END
    END;
    RETURN accum
  END ScanDelimited;

PROCEDURE Format(a : T; base : PrintBase) : TEXT =
  VAR
    c := NEW(CharSeq.T).init();
    s := Sign(a);
    wx := Wx.New();
    MyExtract := extractbase[base];
  BEGIN
    a := Abs(a);

    WHILE NOT Equal(a,Zero) DO
      VAR
        d : T;
      BEGIN
        
        DivideUnsigned(a, MyExtract, a, d);

        <* ASSERT Compare(d,Zero) >= 0 AND Compare(d,MyExtract) < 1 *>
        
        VAR
          toPrint := d.rep.a[0];
        BEGIN
          FOR i := 0 TO chunkdigits[base] - 1 DO
            IF toPrint = 0 AND a = Zero THEN
              EXIT
            END;
            c.addlo(HexChars[toPrint MOD base]);
            toPrint := toPrint DIV base;
          END
        END
      END
    END;

    IF c.size() = 0 THEN
      CASE s OF
        0, 1 => RETURN "0"
      |
        -1   => RETURN "-0"
      END
    END;
      
    IF s = -1 THEN c.addlo('-') END;
    
    FOR i := 0 TO c.size() - 1 DO
      Wx.PutChar(wx, c.get(i))
    END;

    RETURN Wx.ToText(wx)
  END Format;

PROCEDURE FormatOld(a : T; base : PrintBase) : TEXT =
  VAR
    c := NEW(CharSeq.T).init();
    s := Sign(a);
    wx := Wx.New();
    MyBase := small[base];
  BEGIN

    WHILE NOT Equal(a,Zero) DO
      VAR
        d : T;
      BEGIN
        DivideUnsigned(a, MyBase, a, d);
        <* ASSERT Compare(d,Zero) >= 0 AND Compare(d,MyBase) < 1 *>
        c.addlo(HexChars[d.rep.a[0]]);
      END
    END;

    IF c.size() = 0 THEN
      CASE s OF
        0, 1 => RETURN "0"
      |
        -1   => RETURN "-0"
      END
    END;
      
    IF s = -1 THEN c.addlo('-') END;
    
    FOR i := 0 TO c.size() - 1 DO
      Wx.PutChar(wx, c.get(i))
    END;

    RETURN Wx.ToText(wx)
  END FormatOld;

PROCEDURE Hash(a : T) : Word.T = 
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := 0 TO a.rep.siz - 1 DO
      res := Word.Plus(res, a.rep.a[i])
    END;
    RETURN res
  END Hash;

PROCEDURE Max(a, b : T) : T =
  BEGIN IF Compare(a,b) = 1 THEN RETURN a ELSE RETURN b END END Max;

PROCEDURE Min(a, b : T) : T = 
  BEGIN IF Compare(a,b) = -1 THEN RETURN a ELSE RETURN b END END Min;

PROCEDURE ToLongReal(a : T) : LONGREAL = 
  BEGIN
    IF a.rep.siz = 0 THEN RETURN 0.0D0; END;
    VAR
      res := FLOAT(a.rep.a[a.rep.siz-1],LONGREAL);
    BEGIN
      FOR i := a.rep.siz - 2 TO 0 BY -1 DO
        res := res * FLOAT(Base,LONGREAL);
        res := res + FLOAT(a.rep.a[i],LONGREAL)
      END;
      RETURN res;
    END;
  END ToLongReal;

PROCEDURE ToInteger(a : T) : INTEGER RAISES { OutOfRange } =
  BEGIN
    IF Compare(a, FirstInt) = -1 OR
      Compare(a, LastInt) = +1 THEN
      RAISE OutOfRange
    END;
    IF a.rep.siz = 0 THEN RETURN 0; END;
    VAR
      res := a.rep.a[a.rep.siz-1];
    BEGIN
      FOR i := a.rep.siz - 2 TO 0 BY -1 DO
        res := res * Base;
        res := res + a.rep.a[i]
      END;
      RETURN res;
    END;
  END ToInteger;

PROCEDURE GetRepBase() : T =
  BEGIN
    RETURN RepBase
  END GetRepBase;

PROCEDURE GetBit(t : T; bit : CARDINAL) : [ 0 .. 1 ] =
  VAR
    unsignedBit : [ 0 .. 1 ];
  BEGIN
    WITH word = bit DIV BaseLog2,
         pos  = bit MOD BaseLog2 DO
      IF word > LAST(t.rep.a^) THEN
        unsignedBit := 0
      ELSE
        unsignedBit := Word.Extract(t.rep.a[word], pos, 1)
      END;

      IF t.sign = 1 THEN
        RETURN unsignedBit
      END;
      
      <*ASSERT t.sign = -1*>

      IF word > LAST(t.rep.a^) THEN
        RETURN 1 (* leading 1 *)
      END;
      
      VAR
        carry : [ 0 .. 1 ] := 1;
      BEGIN
        FOR i := 0 TO word - 1 DO
          IF t.rep.a[i] # 0 THEN
            carry := 0;
            EXIT
          END
        END;
        
        IF Word.Extract(t.rep.a[word], 0, pos) # 0 THEN
          carry := 0
        END;
        
        RETURN Word.And(1 - unsignedBit + carry, 1)
        
      END
    END      
  END GetBit;

PROCEDURE IsT(ref : REFANY) : BOOLEAN =
  BEGIN RETURN ISTYPE(ref, T) END IsT;

VAR
  mu   := NEW(MUTEX);
  uniq := TRUE;
  tbl  := NEW(BigIntBigIntTbl.Default).init();
  
PROCEDURE UniqReferences(to : BOOLEAN) : BOOLEAN =
  VAR
    old : BOOLEAN;
  BEGIN
    LOCK mu DO
      old := uniq;
      uniq := to;
      RETURN old
    END
  END UniqReferences;

PROCEDURE Uniq(t : T) : T =
  BEGIN
    IF NOT uniq THEN
      RETURN t
    END;
    <*ASSERT t.sign # 0*>

    IF NUMBER(t.rep.a^) = 1 AND t.rep.a[0] = 0 THEN
      <*ASSERT NUMBER(t.rep.a^) = 1*>
      <*ASSERT t.rep.a[0] = 0*>
      RETURN Zero
    END;
      

    LOCK mu DO
      VAR
        q : T;
      BEGIN
        IF tbl.get(t, q) THEN
          RETURN q
        ELSE
          EVAL tbl.put(t, t);
          RETURN t
        END
      END
    END
  END Uniq;


CONST WordSize = BITSIZE(Word.T);

TYPE  BitPos = [ 0 .. WordSize - 1 ];

CONST Mask_1  : Word.T = 1;
      Mask_2  : Word.T = Word.Or(Word.Shift(Mask_1, 1), Mask_1);
      Mask_4  : Word.T = Word.Or(Word.Shift(Mask_2, 2), Mask_2);
      Mask_8  : Word.T = Word.Or(Word.Shift(Mask_4, 4), Mask_4);
      Mask16  : Word.T = Word.Or(Word.Shift(Mask_8, 8), Mask_8);
      Mask32  : Word.T = Word.Or(Word.Shift(Mask16,16), Mask16);

      Mask_1H : Word.T = Word.Shift(Mask_1, 1);
      Mask_2H : Word.T = Word.Shift(Mask_2, 2);
      Mask_4H : Word.T = Word.Shift(Mask_4, 4);
      Mask_8H : Word.T = Word.Shift(Mask_8, 8);
      Mask16H : Word.T = Word.Shift(Mask16,16);
      Mask32H : Word.T = Word.Shift(Mask32,32);
      
PROCEDURE FindMsb(w : Word.T) : [ -1..LAST(BitPos) ] =
  VAR
    pos : BitPos := 0;
  BEGIN
    IF w = 0 THEN RETURN -1 END;
    
    IF WordSize = 64 THEN
      IF Word.And(Mask32H, w) # 0 THEN
        INC(pos, 32);
        w := Word.RightShift(w, 32)
      END
    END;
    
    IF Word.And(Mask16H, w) # 0 THEN
      INC(pos, 16);
      w := Word.RightShift(w, 16)
    END;
    IF Word.And(Mask_8H, w) # 0 THEN
      INC(pos,  8);
      w := Word.RightShift(w,  8)
    END;
    IF Word.And(Mask_4H, w) # 0 THEN
      INC(pos,  4);
      w := Word.RightShift(w,  4)
    END;
    IF Word.And(Mask_2H, w) # 0 THEN
      INC(pos,  2);
      w := Word.RightShift(w,  2)
    END;
    IF Word.And(Mask_1H, w) # 0 THEN
      INC(pos,  1);
    END;
    RETURN pos
  END FindMsb;

PROCEDURE GetAbsMsb(t : T) : [ -1 .. LAST(CARDINAL) ] =
  BEGIN
    IF t = Zero THEN
      RETURN -1
    ELSE
      WITH szm1 = t.rep.siz - 1 DO
        RETURN  szm1 * BaseLog2 + FindMsb(t.rep.a[szm1])
      END
    END
  END GetAbsMsb;
  
VAR
  chunkdigits : ARRAY PrintBase OF CARDINAL;
  extractbase : ARRAY PrintBase OF T;

PROCEDURE InitializeFormatHelp() =
  BEGIN
    FOR pb := FIRST(PrintBase) TO LAST(PrintBase) DO
      VAR
        a := 1;
        p := pb;
        op : CARDINAL;
      BEGIN
        REPEAT
          op := p;
          p := p * pb;
          INC(a)
        UNTIL p > Base;
        DEC(a);
        IF doDebug THEN Debug.Out(F("InitializeFormatHelp : printbase %s : p = %s  a = %s",
                    Int(pb), Int(op), Int(a))) END;

        chunkdigits[pb] := a;
        extractbase[pb] := New(op)
      END
    END
  END InitializeFormatHelp;

VAR
  FirstInt, LastInt : T;
  CharVal : ARRAY CHAR OF INTEGER;
  CharValT : ARRAY CHAR OF T;
  small : ARRAY PrintBase OF T;

  RepBase : T;

  Zero := NEW(T, sign := 1, rep := NSeq { 0, NEW(NArry, 1) });
  One  := NEW(T, sign := 1, rep := NSeq { 1, NEW(NArry, 1) });
  Two  := NEW(T, sign := 1, rep := NSeq { 1, NEW(NArry, 1) });

      

  
BEGIN
  <*ASSERT WordSize = 32 OR WordSize = 64*>

  One.rep.a[0] := 1;
  Two.rep.a[0] := 2;

  EVAL Uniq(Zero);
  <*ASSERT Zero = Uniq(Zero)*>
  EVAL Uniq(One);
  <*ASSERT One = Uniq(One)*>
  EVAL Uniq(Two);
  <*ASSERT Two = Uniq(Two)*>

  RepBase  := New(Base);
  FirstInt := New(FIRST(INTEGER));
  LastInt  := New(LAST(INTEGER));
  <*ASSERT Zero = Uniq(Zero)*>

  FOR c := FIRST(CHAR) TO LAST(CHAR) DO
    VAR
      val : INTEGER;
    BEGIN
      CASE c OF
        '0'..'9' => val := ORD(c) - ORD('0')
      |
        'A'..'Z' => val := ORD(c) - ORD('A') + 10
      |
        'a'..'z' => val := ORD(c) - ORD('a') + 10
      ELSE
        val := -1
      END;
      CharVal[c] := val;
      IF val = -1 THEN
        CharValT[c] := NIL
      ELSE
        CharValT[c] := New(val)
      END
    END
  END;
  <*ASSERT Zero = Uniq(Zero)*>

  FOR i := FIRST(PrintBase) TO LAST(PrintBase) DO
    small[i] := New(i)
  END;

  InitializeFormatHelp();
  <*ASSERT Zero = Uniq(Zero)*>

END BigInt.
