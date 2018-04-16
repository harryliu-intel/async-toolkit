MODULE CsrOp;
IMPORT CompAddr, Word;
IMPORT CompRange;
IMPORT Debug;

CONST WordSize = BITSIZE(Word.T);
      
PROCEDURE MakeWrite(at : CompAddr.T; bits : CARDINAL; val : Word.T) : T =
  VAR
    res : T;
  BEGIN
    res.rw := RW.W;
    res.at := at.word;
    res.fv := at.bit;

    IF at.bit + bits < WordSize THEN
      (* single-word case *)
      IF at.bit = 0 THEN
        res.single := val
      ELSE
        res.single := Word.LeftShift(val,at.bit)
      END;
      res.lv := (at.bit + bits - 1);
    ELSE
      (* straddles two words *)
      WITH d = NEW(REF ARRAY OF Word.T, 2) DO
        d[0] := Word.LeftShift(val,at.bit);
        d[1] := Word.RightShift(val,64-at.bit);
        res.data := d
      END;
      res.lv := at.bit + bits - 1 - WordSize
    END;
    res.origin := Origin.Hardware;
    res.hi := Hi(res);
    RETURN res
  END MakeWrite;

PROCEDURE MakeWideWrite(at : CompAddr.T; READONLY val : ARRAY OF [0..1]) : T =
  VAR
    bits := NUMBER(val);
    res : T;
    hi := CompAddr.PlusBits(at, bits-1);
    d := NEW(REF ARRAY OF Word.T, hi.word-at.word+1);
  BEGIN
    res.rw := RW.W;
    res.at := at.word;
    res.fv := at.bit;

    FOR i := FIRST(d^) TO LAST(d^) DO
      d[i] := 0
    END;
    FOR k := 0 TO bits-1 DO
      WITH pos = at.bit + k,
           w = pos DIV WordSize,
           b = pos MOD WordSize DO 
        d[w] := Word.Insert(d[w], val[k], b, 1)
      END
    END;
    res.lv := (at.bit + bits - 1) MOD WordSize;
    res.hi := Hi(res);
    RETURN res
  END MakeWideWrite;

PROCEDURE Hi(t : T) : CompAddr.T =
  BEGIN
    IF t.data = NIL THEN
      RETURN CompAddr.PlusBits(CompAddr.T { t.at, t.lv }, 1 )
    ELSE
      RETURN CompAddr.PlusBits(CompAddr.T { t.at + NUMBER(t.data^) - 1,
                                            t.lv }, 1 )
    END
  END Hi;

PROCEDURE DoField(VAR op : T; d : Word.T; a : CompRange.T) : Word.T =
  VAR
    alo, alim : INTEGER;
  BEGIN
    (* guard against overflow in range calcs : check for word mismatch *)
    IF op.at < a.pos.word OR op.at > a.pos.word + a.wid.word + 1 THEN
      RETURN d
    END;
    
    (* calc "a" range wrt to {op.at,0} *)
    alo  := (a.pos.word-op.at)*Base + a.pos.bit;
    alim := alo + a.wid.word*Base + a.wid.bit;

    IF op.data = NIL THEN
      (* single word *)
      WITH dlo  = MAX(alo, op.fv),
           dlim = MIN(alim, op.lv+1),
           n    = dlim-dlo DO
        IF n <= 0 THEN RETURN d END;
        <*ASSERT dlim <= Base*>
        CASE op.rw OF
          RW.W =>
          IF FALSE THEN Debug.Out("Writing!") END;
          WITH opalign = Word.RightShift(op.single, op.fv) DO
            d := Word.Insert(d, opalign, dlo, n);
            RETURN d
          END
        |
          RW.R =>
          WITH dalign = Word.RightShift(op.single, dlo) DO
            op.single := Word.Insert(op.single, dalign, op.fv, n);
            RETURN d
          END
        END
      END
    ELSE
      (* multiple words *)
      <*ASSERT FALSE*>
    END
  END DoField;

PROCEDURE DoWideField(VAR op : T; VAR d : ARRAY OF [0..1]; a : CompRange.T) =
  VAR
    alo, alim : INTEGER;
  BEGIN
    (* guard against overflow in range calcs : check for word mismatch *)
    IF op.at < a.pos.word OR op.at > a.pos.word + a.wid.word + 1 THEN
      RETURN 
    END;
    
    (* calc "a" range wrt to {op.at,0} *)
    alo  := (a.pos.word-op.at)*Base + a.pos.bit;
    alim := alo + a.wid.word*Base + a.wid.bit;
    
    IF op.data = NIL THEN
      (* single word *)
      WITH dlo  = MAX(alo, op.fv),
           dlim = MIN(alim, op.lv+1),
           n    = dlim-dlo DO
        <*ASSERT dlim <= Base*>
        CASE op.rw OF
          RW.W =>
          WITH opalign = Word.RightShift(op.single, op.fv) DO
            FOR i := dlo TO dlo+n-1 DO
              d[i] := Word.Extract(opalign,i,1)
            END
          END
        |
          RW.R =>
          FOR i := op.fv TO op.lv DO
            op.single := Word.Insert(op.single, d[i-op.fv+dlo], i, 1);
          END
        END
      END
    ELSE
      (* multiple words *)
      <*ASSERT FALSE*>
    END
  END DoWideField;
 
BEGIN END CsrOp.
