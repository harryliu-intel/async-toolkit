MODULE CompRange;
IMPORT CompAddr;
FROM Fmt IMPORT F;
IMPORT Debug;
IMPORT CompAddrSeq;
IMPORT CompAddrPQ;

PROCEDURE PlaceReg(at          : CompAddr.T;         
                   regwidth    : CARDINAL;
                   alignment   : CARDINAL;
                   accesswidth : CARDINAL;
                   addressing  : CompAddr.Addressing;
                   ) : T =
  VAR
    lo : CompAddr.T;
  BEGIN
    (* per the RDL spec...

       if compact, align only to accesswidth

       if regalign or fullalign, align to accesswidth and regwidth

       IN ANY CASE, align to alignment if specified 
    *)

    (* fullalign also affects the layout of arrays!
       not done here! *)
    lo := at;

    <*ASSERT accesswidth MOD 8 = 0*>
    lo := CompAddr.ModAlign(lo, accesswidth DIV 8);

    CASE addressing OF
      CompAddr.Addressing.Compact => (* skip *)
    ELSE
      <*ASSERT regwidth MOD 8 = 0*>
      lo := CompAddr.ModAlign(lo, regwidth DIV 8);
    END;

    IF alignment # CompAddr.Unspecified THEN
      <*ASSERT alignment MOD 8 = 0*>
      <*ASSERT CompAddr.NextPower(alignment) = alignment*>
      lo := CompAddr.ModAlign(lo, alignment DIV 8)
    END;
    RETURN T { lo, CompAddr.PlusBits(lo, regwidth) }
  END PlaceReg;

PROCEDURE Hi(x : T) : CompAddr.T =
  BEGIN RETURN CompAddr.Plus(x.pos, x.wid) END Hi;

PROCEDURE MakeField(at : CompAddr.T; width : CARDINAL) : T =
  BEGIN
    RETURN T { at, CompAddr.FromBits(width) }
  END MakeField;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("{ %s + %s -> %s }",
             CompAddr.Format(a.pos, bytes := FALSE),
             CompAddr.Format(a.wid, bytes := FALSE),
             CompAddr.Format(Hi(a), bytes := FALSE))
  END Format;

REVEAL
  Monotonic = PubMonotonic BRANDED Brand & " Monotonic" OBJECT
    prev := T { CompAddr.Zero, CompAddr.Zero };
    ok := TRUE;
    seq : CompAddrSeq.T;
    min, max : CompAddr.T;
    first := TRUE;
  OVERRIDES
    init     := InitM;
    increase := IncreaseM;
    isok     := IsokM;
    indexArr := IndexArrM;
    setRange := SetRangeM;
  END;

PROCEDURE InitM(m : Monotonic) : Monotonic =
  BEGIN
    m.seq := NEW(CompAddrSeq.T).init();
    RETURN m
  END InitM;

PROCEDURE IncreaseM(m : Monotonic; from : CompAddr.T; to : T) : CompAddr.T =
  BEGIN
    IF m.first THEN
      m.first := FALSE;
      m.min := to.pos;
      m.max := Hi(to)
    ELSE
      m.min := CompAddr.Min(m.min,to.pos);
      m.max := CompAddr.Max(m.max,Hi(to))
    END;
    
    m.seq.addhi(to.pos);
    IF CompAddr.Compare(to.pos, m.prev.pos) = -1 THEN
      (*
      Debug.Out(F("CompRange.IncreaseM : %s -> %s",
                  Format(m.prev),
                  Format(to)));
      *)
      m.ok := FALSE
    END;
    m.prev := to;
    RETURN Hi(to)
  END IncreaseM;

PROCEDURE IsokM(m : Monotonic) : BOOLEAN =
  BEGIN RETURN m.ok END IsokM;

TYPE
  Elt = CompAddrPQ.Elt OBJECT
    idx : CARDINAL;
  END;

PROCEDURE IndexArrM(m : Monotonic) : REF ARRAY OF CARDINAL =
  <*FATAL CompAddrPQ.Empty*>
  VAR
    pq := NEW(CompAddrPQ.Default).init();
    res := NEW(REF ARRAY OF CARDINAL, m.seq.size());
  BEGIN
    FOR i := 0 TO m.seq.size()-1 DO
      WITH elt = NEW(Elt, priority := m.seq.get(i), idx := i) DO
        pq.insert(elt)
      END
    END;

    FOR i := 0 TO m.seq.size()-1 DO
      res[i] := NARROW(pq.deleteMin(),Elt).idx
    END;
    RETURN res
  END IndexArrM;

PROCEDURE SetRangeM(m : Monotonic; VAR min, max : CompAddr.T) =
  BEGIN
    <*ASSERT NOT m.first*>
    min := m.min;
    max := m.max
  END SetRangeM;
  
(**********************************************************************)
  
PROCEDURE From2(lo, lim : CompAddr.T) : T =
  BEGIN
    RETURN T { lo, CompAddr.Minus(lim,lo) }
  END From2;

PROCEDURE Bits(READONLY t : T) : CARDINAL =
  BEGIN
    RETURN t.wid.word * CompAddr.Base + t.wid.bit
  END Bits;
  
BEGIN END CompRange.
