MODULE Polynomial;
IMPORT SortedLongrealMpfrTbl AS LRMTbl;
IMPORT Mpfr;
FROM Fmt IMPORT F, LongReal, Style;
IMPORT Wx;

CONST LR = LongReal;

REVEAL
  T = LRMTbl.Default BRANDED Brand OBJECT END;

  (* the implementation is a sorted table, with the key being the
     power, and the value being the coefficient.

     That is, if the table contains (k,v) this means that the polynomial
     contains a (monomial) term 

                       v x^k

     The polynomial is simply the sum of all monomials.
   *)

PROCEDURE MakeConstant(x : Coefficient) : T =
  VAR
    res := NEW(T).init();
  BEGIN
    EVAL res.put(0.0d0, x);
    RETURN res
  END MakeConstant;

VAR M0, M1, Mn1 : Coefficient;
    One         : T;
    Zero        : T;
    NegOne      : T;
    
PROCEDURE MakePower(x : Exponent) : T =
  VAR
    res := NEW(T).init();
  BEGIN
    EVAL res.put(x, M1);
    RETURN res
  END MakePower;

PROCEDURE MakeMono(c : Coefficient; x : Exponent) : T =
  VAR
    res := NEW(T).init();
  BEGIN
    EVAL res.put(x, c);
    RETURN res
  END MakeMono;

PROCEDURE ScaleExponents(a : T; by : Exponent) : T =
  VAR
    res := NEW(T).init();
    ai : Iterator;
    ax : Exponent;
    ac : Coefficient;
  BEGIN
    <*ASSERT a # NIL*>
    ai := a.iterate();
    WHILE ai.next(ax, ac) DO
      EVAL res.put(ax * by, ac)
    END;
    RETURN res
  END ScaleExponents;
  
PROCEDURE Plus(a, b : T) : T =

  PROCEDURE Put(x : Exponent; c : Coefficient) =
    BEGIN
      IF NOT Mpfr.EqualP(c, M0) THEN
        EVAL res.put(x, c)
      END
    END Put;
    
  VAR
    res := NEW(T).init();
    ai, bi : Iterator;
    ha, hb := FALSE;

    ax, bx : Exponent; (* exponent    *)
    ac, bc : Coefficient;   (* coefficient *)
  BEGIN
    <*ASSERT a # NIL*>
    <*ASSERT b # NIL*>
    ai  := a.iterateOrdered();
    bi  := b.iterateOrdered();
    LOOP
      IF NOT ha AND ai # NIL THEN
        IF ai.next(ax, ac) THEN
          ha := TRUE
        ELSE
          ai := NIL
        END
      END;
      IF NOT hb AND bi # NIL THEN
        IF bi.next(bx, bc) THEN
          hb := TRUE
        ELSE
          bi := NIL
        END
      END;

      IF    NOT ha AND NOT hb THEN
        RETURN res
      ELSIF ha AND NOT hb THEN
        Put(ax, ac);
        ha := FALSE
      ELSIF hb AND NOT ha THEN
        Put(bx, bc);
        hb := FALSE
      ELSIF ax < bx THEN
        Put(ax, ac);
        ha := FALSE
      ELSIF bx < ax THEN
        Put(bx, bc);
        hb := FALSE
      ELSE
        <*ASSERT ax = bx *>
        <*ASSERT ha AND hb *>
        WITH c = MpfrPlus(ac, bc) DO
          IF NOT Mpfr.ZeroP(c) THEN
            Put(ax, c)
          END
        END;
        ha := FALSE;
        hb := FALSE
      END
    END
  END Plus;

PROCEDURE Times(a, b : T) : T =

  PROCEDURE Accum(x : Exponent; c : Coefficient) =
    VAR
      oc : Coefficient;
    BEGIN
      IF res.get(x, oc) THEN
        c := MpfrPlus(oc, c)
      END;
      IF Mpfr.EqualP(c, M0) THEN
        EVAL res.delete(x, oc)
      ELSE
        EVAL res.put(x, c)
      END
    END Accum;
    
  VAR
    res := NEW(T).init();
    ai : Iterator;
    ax : Exponent;
    ac : Coefficient;
  BEGIN
    <*ASSERT a # NIL*>
    <*ASSERT b # NIL*>
    ai := a.iterate();
    WHILE ai.next(ax, ac) DO
      VAR
        bi := b.iterate();
        bx : Exponent;
        bc : Coefficient;
      BEGIN
        WHILE bi.next(bx, bc) DO
          Accum(ax + bx, MpfrTimes(ac, bc))
        END
      END
    END;
    RETURN res
  END Times;

PROCEDURE ZeroTermD(a : T; exponent : Exponent) : T =
  (* destructively zero the given exponent *)
  VAR
    dummy : Coefficient;
  BEGIN
    EVAL a.delete(exponent, dummy);
    RETURN a
  END ZeroTermD;

PROCEDURE ZeroP(a : T) : BOOLEAN =
  VAR
    ai : Iterator;
    ax : Exponent;
    ac : Coefficient;
  BEGIN
    <*ASSERT a # NIL*>
    ai := a.iterate();
    WHILE ai.next(ax, ac) DO
      IF NOT Mpfr.ZeroP(ac) THEN
        RETURN FALSE
      END
    END;
    RETURN TRUE
  END ZeroP;
  
PROCEDURE LongDivide(n, d : T; remOk : BOOLEAN) : DivResult
  RAISES { Remainder, DivisionByZero } =
  VAR
    ni, di : Iterator;
    nx, dx : Exponent;
    nc, dc : Coefficient;
  BEGIN
    <*ASSERT n # NIL*>
    <*ASSERT d # NIL*>
    ni := n.iterateOrdered(up := FALSE);
    di := d.iterateOrdered(up := FALSE);
    WITH gotIt = ni.next(nx, nc) DO
      IF NOT gotIt THEN RETURN DivResult { Zero, Zero } END
    END;
    WITH gotIt = di.next(dx, dc) DO
      IF NOT gotIt OR Mpfr.ZeroP(dc) THEN RAISE DivisionByZero END
    END;

    IF dx > nx THEN
      IF NOT remOk AND NOT ZeroP(n) THEN
        RAISE Remainder
      END;
      RETURN DivResult { Zero, n }
    ELSE
      WITH leadX = nx - dx,
           leadC = MpfrDiv(nc, dc),
           leadT = MakeMono(leadC, leadX),
           
           toSub = Times(leadT, d),
           toAdd = Times(toSub, NegOne),
           
           rem   = ZeroTermD(Plus(n, toAdd), nx),
           (* ZeroTerm fixes roundoff error *)
           
           rest  = LongDivide(rem, d, remOk) DO
        RETURN DivResult { Plus(leadT, rest.quotient), rest.remainder }
      END
    END;
    
  END LongDivide;
  

PROCEDURE IntPow(a : T; k : CARDINAL) : T =
  VAR
    p := a;
    r := One;
  BEGIN
    LOOP
      IF    k = 0 THEN
        RETURN r
      ELSIF k MOD 2 = 1 THEN
        r := Times(r, p)
      END;
      k := k DIV 2;
      p := Times(p, p)
    END
  END IntPow;

PROCEDURE MpfrPlus(a, b : Coefficient) : Coefficient =
  VAR
    res := Mpfr.New(prec);
  BEGIN
    EVAL Mpfr.Add(res, a, b);
    RETURN res
  END MpfrPlus;

PROCEDURE MpfrTimes(a, b : Coefficient) : Coefficient =
  VAR
    res := Mpfr.New(prec);
  BEGIN
    EVAL Mpfr.Mul(res, a, b);
    RETURN res
  END MpfrTimes;
  
PROCEDURE MpfrDiv(a, b : Coefficient) : Coefficient =
  VAR
    res := Mpfr.New(prec);
  BEGIN
    EVAL Mpfr.Div(res, a, b);
    RETURN res
  END MpfrDiv;
  
PROCEDURE MpfrAbs(a : Coefficient) : Coefficient =
  VAR
    res := Mpfr.New(prec);
  BEGIN
    EVAL Mpfr.Abs(res, a);
    RETURN res
  END MpfrAbs;
  
PROCEDURE Iterate(a : T) : Iterator =
  BEGIN RETURN a.iterateOrdered() END Iterate;

VAR prec := DefPrec;

PROCEDURE SetPrec(to : CARDINAL) = BEGIN prec := to END SetPrec;

PROCEDURE GetPrec() : CARDINAL = BEGIN RETURN prec END GetPrec;

PROCEDURE DebugFmt(a : T) : TEXT =
  VAR
    wx := Wx.New();
    ai := a.iterateOrdered();
    ax : Exponent;
    ac : Coefficient;
  BEGIN
    Wx.PutText(wx, "(+ ");
    WHILE ai.next(ax, ac) DO
      Wx.PutText(wx, F("(* %s (^ x %s))\n",
                       Mpfr.Format(ac),
                       LR(ax)))
    END;
    Wx.PutText(wx, ")");
    RETURN Wx.ToText(wx)
  END DebugFmt;

PROCEDURE LaTeXFmt(a : T) : TEXT =
  VAR
    wx := Wx.New();
    ai := a.iterateOrdered(up := FALSE);
    ax : Exponent;
    ac : Coefficient;
    signT : TEXT;
    first := TRUE;
  BEGIN
    WHILE ai.next(ax, ac) DO
      IF Mpfr.Sign(ac) = -1 THEN
        signT := "-"
      ELSIF first THEN
        signT := ""
      ELSE
        signT := "+"
      END;
      first := FALSE;
      Wx.PutText(wx, F("%s %s \\, x ^ { %s } ",
                       signT,
                       Mpfr.FormatInt(MpfrAbs(ac)),
                       LR(ax, style := Style.Fix, prec := 2)))
    END;
    RETURN Wx.ToText(wx)
  END LaTeXFmt;

BEGIN
  M0  := Mpfr.New(DefPrec);
  M1  := Mpfr.New(DefPrec);
  Mn1 := Mpfr.New(DefPrec);

  EVAL Mpfr.SetInt(M0, 0);
  EVAL Mpfr.SetInt(M1, 1);
  EVAL Mpfr.SetInt(Mn1, -1);
  
  One    := MakeConstant(M1);
  Zero   := MakeConstant(M0);
  NegOne := MakeConstant(Mn1);
END Polynomial.
