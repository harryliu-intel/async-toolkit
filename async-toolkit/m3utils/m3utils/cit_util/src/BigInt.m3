(* $Id$ *)

MODULE BigInt;
IMPORT CardSeq, Integer;
IMPORT CharSeq;
IMPORT TextWr;
IMPORT Wr, Thread;
IMPORT Word;

CONST Base = 1000; (* must be less than or equal to sqrt(LAST(CARDINAL)) *)

(********************* sequence of cardinals *********************)

TYPE 
  Seq = CardSeq.T OBJECT METHODS
    clearTop() := ClearTop;
    copy() : Seq := CopySeq;
  OVERRIDES
    put := MyPut; 
    get := MyGet
  END;

PROCEDURE CopySeq(s : Seq) : Seq = 
  VAR 
    res := NEW(Seq).init();
  BEGIN
    FOR i := 0 TO s.size() - 1 DO
      res.put(i,s.get(i))
    END;
    RETURN res
  END CopySeq;

PROCEDURE ClearTop(s : Seq) =
  BEGIN
    FOR i := s.size() - 1 TO 0 BY -1 DO
      IF CardSeq.T.get(s,i) # 0 THEN RETURN END;
      EVAL s.remhi()
    END
  END ClearTop;

PROCEDURE MyPut(s : Seq; i : CARDINAL; READONLY x : CARDINAL) =
  BEGIN
    FOR j := 1 TO i - s.size() + 1 DO s.addhi(0) END;
    CardSeq.T.put(s,i,x);
    s.clearTop();
  END MyPut;

PROCEDURE MyGet(s : Seq; i : CARDINAL) : CARDINAL =
  BEGIN
    TRY
      FOR j := 1 TO i - s.size() + 1 DO s.addhi(0) END;
      RETURN CardSeq.T.get(s,i)
    FINALLY
      s.clearTop()
    END
  END MyGet;

(* other sequence impl *)

TYPE 
  NArry = REF ARRAY OF CARDINAL;

  NSeq = OBJECT
    siz : CARDINAL;
    a : NArry;
  METHODS
    init(hintSize : CARDINAL) : NSeq := InitN;
    shift(sa : CARDINAL) := ShiftN;
    clearTop() := CTN;
    extend(toBits : CARDINAL) := ExtendN;
    copy() : NSeq := CopyN;
    size() : CARDINAL := SizeN;
  END;

PROCEDURE InitN(s : NSeq; hintSize : CARDINAL) : NSeq =
  BEGIN
    s.siz := 0;
    s.a := NEW(NArry, hintSize);
    FOR i := FIRST(s.a^) TO LAST(s.a^) DO s.a[i] := 0 END;
    RETURN s
  END InitN;

PROCEDURE ShiftN(s : NSeq; sa : CARDINAL) =
  BEGIN
    INC(s.siz,sa);
    s.extend(s.siz)
  END ShiftN;

PROCEDURE ExtendN(s : NSeq; toBits : CARDINAL) =
  BEGIN
    IF s.siz < toBits THEN
      VAR 
        na := NEW(NArry,toBits);
      BEGIN
        SUBARRAY(na^,0,NUMBER(s.a^)) := s.a^;

        FOR i := NUMBER(s.a^) TO LAST(na^) DO na[i] := 0 END;

        s.a := na
      END
    END
  END ExtendN;

PROCEDURE SizeN(s : NSeq) : CARDINAL = BEGIN RETURN s.siz END SizeN;

PROCEDURE CopyN(s : NSeq) : NSeq =
  VAR
    na := NEW(NArry, NUMBER(s.a^));
  BEGIN 
    na^ := s.a^;
    RETURN NEW(NSeq, siz := s.siz, a := na) 
  END CopyN;

PROCEDURE CTN(s : NSeq) = 
  BEGIN 
    FOR i := s.siz - 1 TO 0 BY -1 DO
      IF s.a[i] = 0 THEN s.siz := i ELSE EXIT END
    END
  END CTN;

(********************* bignum type *********************)

REVEAL 
  T = Public BRANDED Brand OBJECT
    sign : [-1..1];
    rep : Seq;
  END;

PROCEDURE Compare(a, b : T) : CompRet =
  BEGIN
    IF a.sign < b.sign THEN RETURN -1
    ELSIF a.sign > b.sign THEN RETURN 1
    END;

    IF a.rep.size() > b.rep.size() THEN RETURN a.sign 
    ELSIF a.rep.size() < b.rep.size() THEN RETURN -a.sign
    END;

    FOR i := a.rep.size() - 1 TO 0 BY -1 DO
      VAR
        c := Integer.Compare(a.rep.get(i),b.rep.get(i));
      BEGIN
        IF c # 0 THEN RETURN a.sign * c END
      END
    END;

    RETURN 0
  END Compare;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN Compare(a,b) = 0 END Equal;

PROCEDURE New(x : INTEGER) : T =
  VAR
    c := ABS(x);
    s := NEW(Seq).init();
  BEGIN 
    s.addlo(c);

    Renormalize(s);

    IF x >= 0 THEN
      RETURN NEW(T, sign := 1, rep := s)
    ELSE
      RETURN NEW(T, sign := -1, rep := s)
    END
  END New;

PROCEDURE Sign(a : T) : CompRet = BEGIN RETURN a.sign END Sign;

PROCEDURE Divide(a, b : T; VAR q, r : T) = 
  VAR
    sign := a.sign * b.sign;
    aa := Abs(a);
    bb := Abs(b);
  BEGIN
    DivideUnsigned(aa,bb,q,r);
    IF sign < 0 THEN q := Neg(q); r := Neg(r) END
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
    seq := a.rep.copy();
  BEGIN
    FOR i := 1 TO sa DO seq.addlo(0) END;
    RETURN NEW(T, sign := a.sign, rep := seq)
  END Shift;

PROCEDURE DivideUnsigned(aparm, b : T; VAR q, r : T) =
  VAR
    s : T;
    lo, hi : CARDINAL;
  BEGIN
    q := Zero;
    r := aparm;
    <* ASSERT aparm.sign = 1 AND b.sign = 1 *>
    
    FOR sa := aparm.rep.size() - b.rep.size() TO 0 BY -1 DO
      s := Shift(b,sa);
      
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
          (*<* ASSERT Compare(loM,r) <= 0 AND Compare(hiM,r) >= 0 *>*)
          CASE midC OF
            -1 => lo := mid
          |
             0 => q := Add(Shift(New(mid),sa),q); r := Zero; RETURN
          |
             1 => hi := mid
          END
        END
      END;

      q := Add(Shift(New(lo),sa),q);
      r := Sub(r, Mul(New(lo),s))
    END;
    <* ASSERT Equal(aparm,Add(r,Mul(q,b))) *>
  END DivideUnsigned;

PROCEDURE Mul(a, b : T) : T =
  BEGIN
    RETURN NEW(T, sign := a.sign * b.sign, rep := MulSeqs(a.rep,b.rep))
  END Mul;

PROCEDURE MulSeqs(a, b : Seq) : Seq =
  VAR 
    res := NEW(Seq).init();
    idx : CARDINAL;
    s : CARDINAL;
  BEGIN
    FOR i := 0 TO a.size() - 1 DO
      FOR j := 0 TO b.size() - 1 DO
        idx := i+j;
        s := a.get(i) * b.get(j);
        WHILE s # 0 DO
          VAR
            o := res.get(idx);
          BEGIN
            s := s + o;
            res.put(idx, s MOD Base);
            s := s DIV Base;
            INC(idx)
          END
        END
      END
    END;
    RETURN res
  END MulSeqs;

PROCEDURE Add(a, b : T) : T =
  VAR
    res : T;
  BEGIN
    IF a.sign = 1 AND b.sign = 1 THEN
      res := NEW(T, sign := 1, rep := AddSeqs(a.rep,b.rep))
    ELSIF a.sign = -1 AND b.sign = -1 THEN
      res := NEW(T, sign := -1, rep := AddSeqs(a.rep,b.rep))
    ELSIF a.sign = -1 AND b.sign = 1 THEN
      res := Sub(b,Neg(a))
    ELSIF a.sign = 1 AND b.sign = -1 THEN
      res := Sub(a,Neg(b))
    ELSE
      <* ASSERT FALSE *>
    END;
    
    Renormalize(res.rep);
    RETURN res
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
    RETURN res
  END Sub;

(* unsigned addition of underlying sequences *)
PROCEDURE AddSeqs(s, t : Seq) : Seq =
  VAR
    r := NEW(Seq).init(MAX(s.size(),t.size()));
  BEGIN
    FOR i := 0 TO MAX(s.size(),t.size()) - 1 DO
      r.put(i,s.get(i) + t.get(i))
    END;
    RETURN r
  END AddSeqs;

(* unsigned subtraction of underlying sequences *)
(* s must be .ge. t *)
PROCEDURE SubSeqs(s, t : Seq) : Seq =
  VAR
    r := NEW(Seq).init();
    borrow := 0;
  BEGIN
    FOR i := 0 TO MAX(s.size(),t.size()) - 1 DO
      VAR
        diff := s.get(i) - t.get(i) + borrow;
      BEGIN
        borrow := 0;

        WHILE diff < 0 DO
          diff := diff + Base;
          borrow := borrow - 1
        END;
        r.put(i,diff)
      END
    END;
    <* ASSERT borrow = 0 *>
    RETURN r
  END SubSeqs;

PROCEDURE Abs(a : T) : T = 
  BEGIN RETURN NEW(T, sign := 1, rep := a.rep) END Abs;

PROCEDURE Renormalize(a : Seq) = 
  VAR
    carry := 0;
    o : CARDINAL;
    i := 0;
  BEGIN
    WHILE i < a.size() OR carry # 0 DO
      o := a.get(i) + carry;
      a.put(i,o MOD Base);
      carry := o DIV Base;
      INC(i)
    END;
  END Renormalize;

PROCEDURE Neg(a : T) : T = 
  BEGIN RETURN NEW(T, rep := a.rep, sign := -a.sign) END Neg;

PROCEDURE Format(a : T; base : CARDINAL) : TEXT = 
  <* FATAL Wr.Failure, Thread.Alerted *>
  VAR
    c := NEW(CharSeq.T).init();
    s := Sign(a);
    wr := NEW(TextWr.T).init();
    MyBase := New(base);
  BEGIN
    <* ASSERT base <= 10 *>
    a := Abs(a);

    WHILE NOT Equal(a,Zero) DO
      VAR
        d : T;
      BEGIN
        DivideUnsigned(a,MyBase,a,d);
        <* ASSERT Compare(d,Zero) >= 0 AND Compare(d,MyBase) < 1 *>
        c.addlo(VAL(ORD('0') + d.rep.get(0), CHAR))
      END
    END;
    IF s = -1 THEN c.addlo('-') END;

    FOR i := 0 TO c.size() - 1 DO
      Wr.PutChar(wr, c.get(i))
    END;

    RETURN TextWr.ToText(wr)
  END Format;

PROCEDURE Hash(a : T) : Word.T = 
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := 0 TO a.rep.size() - 1 DO
      res := Word.Plus(res,a.rep.get(i))
    END;
    RETURN res
  END Hash;

PROCEDURE Max(a, b : T) : T =
  BEGIN IF Compare(a,b) = 1 THEN RETURN a ELSE RETURN b END END Max;

PROCEDURE Min(a, b : T) : T = 
  BEGIN IF Compare(a,b) = -1 THEN RETURN a ELSE RETURN b END END Min;

BEGIN 
  Zero := New(0);
  One := New(1);
END BigInt.
