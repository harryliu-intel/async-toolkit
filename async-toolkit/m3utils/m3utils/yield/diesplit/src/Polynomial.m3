MODULE Polynomial;
IMPORT SortedLongrealMpfrTbl AS LRMTbl;
IMPORT Mpfr;
FROM Fmt IMPORT F, LongReal;
IMPORT Wx;

CONST LR = LongReal;

REVEAL
  T = LRMTbl.Default BRANDED Brand OBJECT END;

PROCEDURE MakeConstant(x : Mpfr.T) : T =
  VAR
    res := NEW(T).init();
  BEGIN
    EVAL res.put(0.0d0, x);
    RETURN res
  END MakeConstant;

VAR M0, M1 : Mpfr.T;
    One    : T;
  
PROCEDURE MakePower(x : LONGREAL) : T =
  VAR
    res := NEW(T).init();
  BEGIN
    EVAL res.put(x, M1);
    RETURN res
  END MakePower;

PROCEDURE Plus(a, b : T) : T =

  PROCEDURE Put(x : LONGREAL; c : Mpfr.T) =
    BEGIN
      IF NOT Mpfr.EqualP(c, M0) THEN
        EVAL res.put(x, c)
      END
    END Put;
    
  VAR
    res := NEW(T).init();
    ai  := a.iterateOrdered();
    bi  := b.iterateOrdered();
    ha, hb := FALSE;

    ax, bx : LONGREAL; (* exponent    *)
    ac, bc : Mpfr.T;   (* coefficient *)
  BEGIN
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
        Put(ax, MpfrPlus(ac, bc));
        ha := FALSE;
        hb := FALSE
      END
    END
  END Plus;

PROCEDURE Times(a, b : T) : T =

  PROCEDURE Accum(x : LONGREAL; c : Mpfr.T) =
    VAR
      oc : Mpfr.T;
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
    ai := a.iterate();
    ax : LONGREAL;
    ac : Mpfr.T;
  BEGIN
    WHILE ai.next(ax, ac) DO
      VAR
        bi := b.iterate();
        bx : LONGREAL;
        bc : Mpfr.T;
      BEGIN
        WHILE bi.next(bx, bc) DO
          Accum(ax + bx, MpfrTimes(ac, bc))
        END
      END
    END;
    RETURN res
  END Times;

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

PROCEDURE MpfrPlus(a, b : Mpfr.T) : Mpfr.T =
  VAR
    res := Mpfr.New(prec);
  BEGIN
    EVAL Mpfr.Add(res, a, b);
    RETURN res
  END MpfrPlus;
  
PROCEDURE MpfrTimes(a, b : Mpfr.T) : Mpfr.T =
  VAR
    res := Mpfr.New(prec);
  BEGIN
    EVAL Mpfr.Mul(res, a, b);
    RETURN res
  END MpfrTimes;
  
PROCEDURE Iterate(a : T) : Iterator =
  BEGIN RETURN a.iterateOrdered() END Iterate;

VAR prec := DefPrec;

PROCEDURE SetPrec(to : CARDINAL) = BEGIN prec := to END SetPrec;

PROCEDURE GetPrec() : CARDINAL = BEGIN RETURN prec END GetPrec;

PROCEDURE DebugFmt(a : T) : TEXT =
  VAR
    wx := Wx.New();
    ai := a.iterateOrdered();
    ax : LONGREAL;
    ac : Mpfr.T;
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
  
BEGIN
  M0 := Mpfr.New(DefPrec);
  M1 := Mpfr.New(DefPrec);
  EVAL Mpfr.SetInt(M0, 0);
  EVAL Mpfr.SetInt(M1, 1);
  One := MakeConstant(M1);
END Polynomial.
