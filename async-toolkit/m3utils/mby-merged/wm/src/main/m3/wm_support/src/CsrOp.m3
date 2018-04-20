MODULE CsrOp;
IMPORT CompAddr, Word;
IMPORT CompRange;
IMPORT Debug;
IMPORT Fmt;
FROM Fmt IMPORT F;

PROCEDURE MakeWrite(at : CompAddr.T; bits : CARDINAL; val : Word.T) : T =
  VAR
    res : T;
  BEGIN
    res.rw := RW.W;
    res.at := at.word;
    res.fv := at.bit;

    IF at.bit + bits <= Base THEN
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
      res.lv := at.bit + bits - 1 - Base
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
           w = pos DIV Base,
           b = pos MOD Base DO 
        d[w] := Word.Insert(d[w], val[k], b, 1)
      END
    END;
    res.lv := (at.bit + bits - 1) MOD Base;
    res.hi := Hi(res);
    RETURN res
  END MakeWideWrite;

PROCEDURE Hi(t : T) : CompAddr.T =
  BEGIN
    IF t.lv = -1 THEN
      (* special case, zero bits *)
      <*ASSERT t.data = NIL OR NUMBER(t.data^) = 1*> 
      RETURN CompAddr.T { t.at, 0 }
    END;
    IF t.data = NIL THEN
      RETURN CompAddr.PlusBits(CompAddr.T { t.at, t.lv }, 1 )
    ELSE
      RETURN CompAddr.PlusBits(CompAddr.T { t.at + NUMBER(t.data^) - 1,
                                            t.lv }, 1 )
    END
  END Hi;

PROCEDURE DoField(VAR op      : T;          (* the read/write op *)
                  d           : Word.T;     (* the old value of the field *)
                  READONLY a  : CompRange.T (* the address range of the field *)
  ) : Word.T = (* the new value of the field *)

  PROCEDURE Check(q : Word.T) =
    BEGIN
      WITH n = CompRange.Bits(a),
           m = Word.Shift(1,n)-1, (* field mask *)
           i = Word.Not(m),       (* inverse of field mask *)
           r = Word.And(i,q)      (* there should be no overlap *)
       DO
        IF n # Base THEN
          IF r # 0 THEN
            Debug.Error(F("CompRange %s : bits %s lim 16_%s <= res 16)_%s",
                          CompRange.Format(a),
                          Fmt.Int(n),
                          Fmt.Unsigned(m, base := 16),
                          Fmt.Unsigned(res, base := 16)))
          END;
          <*ASSERT r = 0*>
        END
      END
    END Check;
    
  VAR
    alo, alim : INTEGER;
    res : Word.T;
  BEGIN
    (* guard against overflow in range calcs : check for word mismatch *)
    IF op.at < a.pos.word OR op.at > a.pos.word + a.wid.word + 1 THEN
      res := d;
      Check(res);
      RETURN res
    END;
    
    (* calc "a" range wrt to {op.at,0} *)
    (* this is valid for both single-word and multi-word writes *)
    alo  := (a.pos.word-op.at)*Base + a.pos.bit;
    alim := alo + a.wid.word*Base + a.wid.bit;

    IF op.data = NIL THEN
      (* single word *)
      WITH dlo  = MAX(alo, op.fv),
           dlim = MIN(alim, op.lv+1),
           n    = dlim-dlo DO
        IF n <= 0 THEN
          res := d;
          Check(res);
          RETURN res
        END;
        <*ASSERT dlim <= Base*>
        <*ASSERT n<=CompRange.Bits(a)*>
        CASE op.rw OF
          RW.W =>
          IF FALSE THEN Debug.Out("Writing!") END;
          WITH opalign = Word.RightShift(op.single, op.fv) DO
            d := Word.Insert(d, opalign, dlo-alo, n);
            res := d;
            Check(res);
            RETURN res
          END
        |
          RW.R =>
          WITH dalign = Word.RightShift(op.single, dlo) DO
            op.single := Word.Insert(op.single, dalign, op.fv, n);
            res := d;
            Check(res);
            RETURN res
          END
        END
      END
    ELSE
      (* multiple words *)
      WITH dlo  = MAX(alo, op.fv),    (* in bits *)
           dlim = MIN(alim, (NUMBER(op.data^)-1)*Base+op.lv+1),
           dhi  = dlim-1,
           n    = dlim-dlo,

           wlo = dlo DIV Base,
           blo = dlo MOD Base,
           whi = dhi DIV Base,
           bhi = dhi MOD Base DO
        IF n <= 0 THEN
          res := d;
          Check(res);
          RETURN res
        END;

        (* here n > 0 and we are going to write ... *)

        <*ASSERT whi = wlo OR whi = wlo + 1 *>

        IF whi = wlo THEN
          (* field contained entirely in one word *)
          CASE op.rw OF
            RW.W =>
            res := Word.Extract(op.data[wlo], blo, bhi-blo+1)
          |
            RW.R =>
            op.data[wlo] := Word.Insert(op.data[wlo], d, blo, bhi-blo+1)
          END;
          Check(res);
          RETURN res
        ELSE
          (* field straddles two words *)
          WITH bhi = dhi MOD Base DO
            CASE op.rw OF
              RW.W =>
              WITH
                lo = Word.Extract(op.data[wlo], blo, Base-blo),
                hi = Word.Extract(op.data[whi], 0, bhi+1) DO
                
                res := Word.Or(lo, Word.LeftShift(hi,Base-blo))
              END
            |
              RW.R =>
              op.data[wlo] := Word.Insert(op.data[wlo], d, blo, Base-blo);
              op.data[whi] := Word.Insert(op.data[whi],
                                          Word.RightShift(d,Base-blo),
                                          0,
                                          bhi+1)
            END;
            Check(res);
            RETURN res
          END
        END
      END
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
    
    (* calc "a" range wrt to {op.at,0} -- 

       i.e., d[0] is at pos alo above the base of op.at
    *)
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
              d[i-alo] := Word.Extract(opalign,i,1)
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

      PROCEDURE DoBit(s, w, b : CARDINAL) =
        BEGIN
          CASE op.rw OF
            RW.W =>
            d[s-alo] := Word.Extract(op.data[w], b, 1)
          |
            RW.R =>
            op.data[w] := Word.Insert(op.data[w], d[s-alo], b, 1)
          END
        END DoBit;
        
      VAR
        dlo  := MAX(alo, op.fv);    (* in bits *)
        dlim := MIN(alim, (NUMBER(op.data^)-1)*Base+op.lv+1);
        dhi  := dlim-1;
        n    := dlim-dlo;
        
        wlo := dlo DIV Base;
        blo := dlo MOD Base;
        whi := dhi DIV Base;
        bhi := dhi MOD Base;
      BEGIN
        IF n <= 0 THEN
          RETURN
        END;

        (* here n > 0 and we are going to write ... *)

        <*ASSERT whi = wlo OR whi = wlo + 1 *>

        IF whi = wlo THEN
          (* one word *)
          FOR b := blo TO bhi DO
            DoBit(b, b, wlo)
          END
        ELSE
          (* field straddles two words *)
          FOR w := wlo TO whi DO
            IF w = wlo THEN
              FOR b := blo TO Base-1 DO
                DoBit(Base*w + b, w, b)
              END
            ELSIF w = whi THEN
              FOR b := 0 TO bhi DO
                DoBit(Base*w + b, w, b)
              END
            ELSE
              FOR b := 0 TO Base-1 DO
                DoBit(Base*w + b, w, b)
              END
            END
          END
        END
      END
    END
  END DoWideField;

PROCEDURE LowAddr(t : T) : CompAddr.T =
  BEGIN RETURN CompAddr.T { t.at, t.fv } END LowAddr;
  
BEGIN END CsrOp.
