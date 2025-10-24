MODULE Valenv;
IMPORT TextSrcTbl;
IMPORT AssertionList;
IMPORT X01;
IMPORT NodeRecSeq;
IMPORT AssertionListSeq;
IMPORT Dims;
IMPORT Bit;
IMPORT Assertion;
IMPORT Src;
IMPORT MemoTranSeq;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F;

REVEAL
  T = Public BRANDED OBJECT
    tbl        : TextSrcTbl.T;
    curTm      : LONGREAL;
    dutName    : TEXT;
    
    buf        : AssertionListSeq.T;

    assertions : AssertionList.T;
    minV, maxV : ARRAY Bit.T OF LONGREAL;
  OVERRIDES 
    init := Init;

    setTime := SetTime;
    getTime := GetTime;
    getSngl := GetSngl;
    getArry := GetArry;

    knownOutput := KnownOutput;
    getAssertions := GetAssertions;
  END;

PROCEDURE Init(t : T; dutName : TEXT; srcs : NodeRecSeq.T; lims : Lims) : T =
  BEGIN
    t.dutName := dutName;
    t.curTm := FIRST(LONGREAL);
    t.buf := NEW(AssertionListSeq.T).init();
    t.minV := ARRAY Bit.T OF LONGREAL { lims.minLo, lims.minHi };
    t.maxV := ARRAY Bit.T OF LONGREAL { lims.maxLo, lims.maxHi };
    t.tbl := NEW(TextSrcTbl.Default).init();
    FOR i := 0 TO srcs.size()-1 DO
      WITH s = srcs.get(i),
           src = s.nds.intf DO
        IF ISTYPE(src, Src.T) THEN 
          <*ASSERT s.nds # NIL*>
          <*ASSERT s.nds.nm # NIL*>
          Debug.Out("Valenv.Init adding source " & s.nds.nm);
          EVAL t.tbl.put(s.nds.nm, src) 
        END
      END
    END;

    RETURN t
  END Init;

PROCEDURE GetTime(t : T) : LONGREAL = BEGIN RETURN t.curTm END GetTime;

PROCEDURE SetTime(t : T; tm : LONGREAL) = 
  BEGIN 
    <*ASSERT tm > t.curTm *>
    t.curTm := tm;
    
    (* flush waiting assertions *)
    IF t.buf.size() # 0 THEN
      VAR
        p : AssertionList.T := t.buf.remlo();
      BEGIN
        WHILE p # NIL DO
          p.head.tm := t.curTm + p.head.offset;
          t.assertions := AssertionList.Cons(p.head, t.assertions);
          p := p.tail
        END
      END
    END
  END SetTime;

PROCEDURE GetSngl(t : T; nm : TEXT) : X01.T = 
  CONST
    idx = Dims.T { };
  VAR
    src : Src.T;
  BEGIN
    nm := t.dutName & "." & nm;
    WITH hadIt = t.tbl.get(nm, src) DO 
      IF NOT hadIt THEN
        Debug.Error("Valenv.GetSngl : unknown source \"" & nm & "\"")
      END
    END;

    WITH v = MemoTranSeq.Interpolate(src.getSeq(idx),t.curTm) DO
      IF    v >= t.minV[0] AND v <= t.maxV[0] THEN
        RETURN X01.T.V0
      ELSIF v >= t.minV[0] AND v <= t.maxV[1] THEN
        RETURN X01.T.V1
      ELSE
        RETURN X01.T.VX
      END
    END
  END GetSngl;

PROCEDURE GetArry(t : T; nm : TEXT) : REF ARRAY OF X01.T = 
  VAR
    src : Src.T;
    res : REF ARRAY OF X01.T;
  BEGIN
    nm := t.dutName & "." & nm;
    WITH hadIt = t.tbl.get(nm, src) DO <*ASSERT hadIt*> END;
    WITH dims = src.nodes.dims DO
      IF NUMBER(dims^) # 1 THEN
        Debug.Error(F("Valenv.GetArry: attempting to get linear array \"%s\" of dimension %s", nm, Fmt.Int(NUMBER(dims^))))
      END;
      res := NEW(REF ARRAY OF X01.T, dims[0]);
      FOR i := 0 TO dims[0]-1 DO
        WITH v = MemoTranSeq.Interpolate(src.getSeq(Dims.T { i } ),t.curTm) DO
          IF    v >= t.minV[0] AND v <= t.maxV[0] THEN
            res[i] := X01.T.V0
          ELSIF v >= t.minV[0] AND v <= t.maxV[1] THEN
            res[i] := X01.T.V1
          ELSE
            res[i] := X01.T.VX
          END
        END
      END
    END;
    RETURN res
  END GetArry;

PROCEDURE KnownOutput(t            : T;
                      nm           : TEXT; 
                      READONLY idx : Dims.T; 
                      dly          : CARDINAL; 
                      v            : Bit.T;
                      holdOffset   : LONGREAL) =
  BEGIN
    dly := dly-1; (* asserts that dly >= 1 *)

    WHILE t.buf.size()-1 < dly DO t.buf.addhi(NIL) END;

    <*ASSERT holdOffset > -1.0d10 *>
    <*ASSERT holdOffset < +1.0d10 *>
    
    t.buf.put(dly, AssertionList.Cons(Assertion.T { nm & Dims.Format(idx),
                                                    FIRST(LONGREAL), (* dummy *)
                                                    t.minV[v],
                                                    t.maxV[v],
                                                    holdOffset },
                                      t.buf.get(dly)))
  END KnownOutput;

PROCEDURE GetAssertions(t : T) : AssertionList.T =
  BEGIN RETURN t.assertions END GetAssertions;

BEGIN END Valenv.
