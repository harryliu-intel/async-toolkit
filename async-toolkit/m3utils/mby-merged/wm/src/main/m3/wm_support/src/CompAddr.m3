MODULE CompAddr;
IMPORT Word;
IMPORT Debug;
FROM Fmt IMPORT F, Int;

PROCEDURE Plus(a, b : T) : T =
  VAR
    low : Word.T := a.bit + b.bit;
    carry : Word.T;
    res : T;
  BEGIN
    carry := low DIV Base;

    res.bit := low MOD Base;
    res.word := a.word + b.word + carry;
    RETURN res
  END Plus;

PROCEDURE Minus(m, s : T) : T =
  VAR
    h : Word.T := m.word - s.word;
    res : T;
  BEGIN
    <*ASSERT Compare(m,s) >= 0 *>
    IF s.bit > m.bit THEN
      (* borrow *)
      res.word := h - 1;
      res.bit := m.bit - s.bit + Base
    ELSE
      res.word := h;
      res.bit := m.bit - s.bit
    END;
    IF Plus(res,s) # m THEN
      Debug.Error(F("CompAddr.Minus : internal consistency check: Plus(%s,%s) # %s",
                  Format(res), Format(s), Format(m)))
    END;
    <*ASSERT Plus(res,s) = m*>
    RETURN res
  END Minus;

PROCEDURE PlusBytes(a : T; bytes : CARDINAL) : T =
  BEGIN
    RETURN PlusBits(a, 8 * bytes)
  END PlusBytes;

PROCEDURE PlusBits(augend : T; bits : CARDINAL) : T =
  VAR
    addend := T { bits DIV Base, bits MOD Base };
  BEGIN
    RETURN Plus(augend, addend)
  END PlusBits;

PROCEDURE ModAlign(at : T; byteMod : CARDINAL) : T =
  BEGIN
    <*ASSERT byteMod # 0*>
    (* find next byte *)
    WHILE at.bit MOD 8 # 0 DO
      at := Plus(at, T { 0, 1 })
    END;
    WHILE BitMod(at, byteMod * 8) # 0 DO
      at := Plus(at, T { 0, 8 })
    END;
    RETURN at
  END ModAlign;

PROCEDURE BitMod(a : T; mod : CARDINAL) : CARDINAL =
  VAR
    woM, baM, biM : CARDINAL;
  BEGIN
    <*ASSERT mod # 0*>
    woM := a.word MOD mod;
    baM := Base MOD mod;
    biM := a.bit MOD mod;

    (*  ( word * BITSIZE(Word.T) + bit ) MOD mod 
      =
        (  word MOD mod * BITSIZE(Word.T) MOD mod + bit MOD mod ) MOD mod
    *)
    RETURN (woM * baM + biM) MOD mod
  END BitMod;
  
PROCEDURE FromBytes(bytes : CARDINAL) : T =
  BEGIN RETURN PlusBytes(Zero, bytes) END FromBytes;

PROCEDURE FromBits(bits : CARDINAL) : T =
  BEGIN RETURN PlusBits(Zero, bits) END FromBits;

PROCEDURE DeltaBytes(a, b : T) : CARDINAL =
  VAR
    dw := a.word - b.word;
    db := a.bit  - b.bit;
    delta := dw * Base + db;
  BEGIN
    IF    delta < 0 THEN
      Debug.Error(F("Negative address delta %s - %s", Format(a), Format(b)));
      <*ASSERT FALSE*>
    ELSIF delta MOD 8 # 0 THEN
      Debug.Error(F("Bits remainder in address delta %s - %s", Format(a), Format(b)));
      <*ASSERT FALSE*>
    ELSE
      RETURN delta DIV 8
    END
  END DeltaBytes;

PROCEDURE NextPower(q : CARDINAL) : CARDINAL =
  VAR
    z := 1;
  BEGIN
    IF q = 0 THEN RETURN 0 END;
    WHILE z < q DO
      z := Word.LeftShift(z,1)
    END;
    <*ASSERT z >= q*>
    RETURN z
  END NextPower;

PROCEDURE Format(t : T; bytes : BOOLEAN := FALSE) : TEXT =
  BEGIN
    IF bytes THEN
      RETURN F("{ word %s , bit %s }", Int(t.word * Base + t.bit DIV 8), Int(t.bit MOD 8))
    ELSE
      RETURN F("{ word %s , bit %s }", Int(t.word), Int(t.bit))
    END
  END Format;

PROCEDURE Max(a, b : T) : T =
  BEGIN
    IF    a.word > b.word THEN RETURN a
    ELSIF b.word > a.word THEN RETURN b
    ELSIF a.bit  > b.bit  THEN RETURN a
    ELSIF b.bit  > a.bit  THEN RETURN b
    ELSE
      <*ASSERT a=b*>
      RETURN a
    END
  END Max;
  
PROCEDURE Min(a, b : T) : T =
  BEGIN
    IF    a.word < b.word THEN RETURN a
    ELSIF b.word < a.word THEN RETURN b
    ELSIF a.bit  < b.bit  THEN RETURN a
    ELSIF b.bit  < a.bit  THEN RETURN b
    ELSE
      <*ASSERT a=b*>
      RETURN a
    END
  END Min;

PROCEDURE Compare(a, b : T) : [-1..1] =
  BEGIN
    IF    a.word < b.word THEN RETURN -1
    ELSIF b.word < a.word THEN RETURN +1
    ELSIF a.bit  < b.bit  THEN RETURN -1
    ELSIF b.bit  < a.bit  THEN RETURN +1
    ELSE
      <*ASSERT a=b*>
      RETURN 0
    END
  END Compare;

PROCEDURE Find(READONLY a : ARRAY OF T; tgt : T) : [-1..LAST(CARDINAL)] =

  PROCEDURE Check(i : CARDINAL) : CARDINAL =
    BEGIN
      IF    i = -1 THEN
        (*<*ASSERT Compare(tgt,a[0]) = 1*>*)
      ELSE
        (*<* ASSERT Compare(tgt,a[i]) >= 0 *>*)
        IF i # LAST(a) THEN
          (*<* ASSERT Compare(tgt,a[i+1]) = -1  *>*)
        END
      END;
      RETURN i
    END Check;
    
  VAR
    lo := 0;
    hi := LAST(a);
  BEGIN

    IF Compare(tgt,a[lo]) < 0 (* tgt < a[lo] *) THEN
      RETURN -1
    END;
    (* tgt > a[lo] *)
    
    IF Compare(tgt,a[hi]) >= 0 (* tgt >=  a[hi] *) THEN
      RETURN Check(hi)
    END;
    (* tgt < a[hi] *)

    LOOP
      (*<* ASSERT Compare(tgt,a[lo]) >= 0 AND Compare(tgt,a[hi]) < 0  *>*)
      WITH mid = (lo + hi) DIV 2,
           m   = a[mid] DO

        IF hi = lo + 1 THEN
          RETURN Check(lo)
        END;
        
        (*<*ASSERT mid > lo*>*)
        (*<*ASSERT mid < hi*>*)
        CASE Compare(m,tgt) OF
          +1 => hi := mid (* a[hi] > tgt *)
        |
          -1 => lo := mid (* a[lo] < tgt *)
        |
           0 => RETURN Check(mid)
        END
      END
    END
  END Find;

PROCEDURE FindIndirect(READONLY a : ARRAY OF T;
                       READONLY c : ARRAY OF CARDINAL;
                       tgt : T) : [-1..LAST(CARDINAL)] =

  PROCEDURE Check(i : CARDINAL) : CARDINAL =
    BEGIN
      IF    i = -1 THEN
        (*<*ASSERT Compare(tgt,a[c[0]]) = 1*>*)
      ELSE
        (*<* ASSERT Compare(tgt,a[c[i]]) >= 0 *>*)
        IF i # LAST(a) THEN
          (*<* ASSERT Compare(tgt,a[c[i+1]]) = -1  *>*)
        END
      END;
      RETURN i
    END Check;
    
  VAR
    lo := 0;
    hi := LAST(a);
  BEGIN
    (*<*ASSERT NUMBER(a) = NUMBER(c)*>*)
    IF Compare(tgt,a[c[lo]]) < 0 (* tgt < a[c[lo]] *) THEN
      RETURN -1
    END;
    (* tgt > a[c[lo]] *)
    
    IF Compare(tgt,a[c[hi]]) >= 0 (* tgt >=  a[c[hi]] *) THEN
      RETURN Check(hi)
    END;
    (* tgt < a[c[hi]] *)

    LOOP
      (*<* ASSERT Compare(tgt,a[c[lo]]) >= 0 AND Compare(tgt,a[c[hi]]) < 0  *>*)
      WITH mid = (lo + hi) DIV 2,
           m   = a[c[mid]] DO

        IF hi = lo + 1 THEN
          RETURN Check(lo)
        END;
        
        (*<*ASSERT mid > lo*>*)
        (*<*ASSERT mid < hi*>*)
        CASE Compare(m,tgt) OF
          +1 => hi := mid (* a[c[hi]] > tgt *)
        |
          -1 => lo := mid (* a[c[lo]] < tgt *)
        |
           0 => RETURN Check(mid)
        END
      END
    END
  END FindIndirect;

BEGIN END CompAddr.
