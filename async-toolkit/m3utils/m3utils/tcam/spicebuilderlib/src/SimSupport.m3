MODULE SimSupport;
IMPORT Dims;
IMPORT BitInteger;
IMPORT MemoTranSeq;
IMPORT Src;
IMPORT Intf;
IMPORT Nodes, Debug;
IMPORT Tran;
FROM Fmt IMPORT F; IMPORT Fmt;
IMPORT RefSeq;
IMPORT DimsTranSeqTbl;
IMPORT SimDumper;
FROM SimDumper IMPORT Vdd;
FROM Dims IMPORT Scalar;
IMPORT StandardSettings; FROM StandardSettings IMPORT Constraints;
IMPORT TextSeq, TextReader;

CONST LR = Fmt.LongReal;

REVEAL
  IntegerSrc = PubIntegerSrc BRANDED OBJECT 
  OVERRIDES
    complete := CompleteIntIntf;
    makeSeq  := IntegerMakeSeq;
  END;
  
  ClockedSrcIntf = PubClockedSrcIntf BRANDED OBJECT END;

  RMutexSrc  = PubRMutexSrc BRANDED OBJECT 
  OVERRIDES
    complete := CompleteRMuSrc;
    makeSeq  := RMMakeSeq;
  END;

  ArbSrc  = ClockedSrcIntf BRANDED OBJECT 
  OVERRIDES
    makeSeq := ArbMakeSeq;
  END;

  SeqSrc  = PubSeqSrc BRANDED OBJECT 
  OVERRIDES
    complete := CompleteSeqSrc;
    makeSeq := SeqMakeSeq;
  END;

  ClockedRdrIntf = PubClockedRdrIntf BRANDED OBJECT END;

  ClockSrc   = PubClockSrc BRANDED OBJECT 
  OVERRIDES
    makeSeq := ClockMakeSeq;
    infinite := CSInfinite;
  END;
  
PROCEDURE GetCtrHold(s : StandardSettings.T; x : REF Constraints) : LONGREAL =
  BEGIN
    IF x = NIL THEN RETURN s.defConstraints.hold ELSE RETURN x.hold END
  END GetCtrHold;

PROCEDURE CSInfinite(<*UNUSED*>cs : ClockSrc) : BOOLEAN = 
  BEGIN RETURN TRUE END CSInfinite;

PROCEDURE IntegerMakeSeq(self         : IntegerSrc; 
                         READONLY idx : Dims.T) : MemoTranSeq.T =
  VAR
    res := NEW(MemoTranSeq.T).init();
    v : LONGREAL;
    bit : CARDINAL;
  BEGIN
    Debug.Out("IntegerMakeSeq " & self.nodes.nm & Dims.Format(idx));
    <*ASSERT NUMBER(idx) <= 1*>
    CASE NUMBER(idx) OF
      0 => bit := 0
    |
      1 => bit := idx[0]
    ELSE
      <*ASSERT FALSE*>
    END;
    IF NARROW(self.val,BitInteger.Concrete).bits[bit] = 0 THEN
      v := 0.0d0
    ELSE
      v := Vdd
    END;
    res.addhi(Tran.T { t := 0.0d0, v := v, rf := self.s.riseFall });
    RETURN res
  END IntegerMakeSeq;

PROCEDURE ClockMakeSeq(self : ClockSrc; READONLY idx : Dims.T) : MemoTranSeq.T =
  VAR
    res := NEW(MemoTranSeq.T).init();
    v := self.lo;
    t := 0.0d0;
  BEGIN
    <*ASSERT idx = Scalar*>
    WHILE t < self.s.simParams.maxTime DO
      res.addhi(Tran.T { t := t, v := v, rf := self.s.riseFall });
      t := t + 1.0d0/self.spd/2.0d0; (* half cycle time *)
      <*ASSERT v = 0.0d0 OR v = Vdd*>
      IF v = self.lo THEN v := self.hi ELSE v := self.lo END;
    END;
    RETURN res
  END ClockMakeSeq;

PROCEDURE ArbMakeSeq(self                   : ArbSrc; 
                     <*UNUSED*>READONLY idx : Dims.T) : MemoTranSeq.T =
  VAR
    res := NEW(MemoTranSeq.T).init();
    v := LAST(LONGREAL);
    w := 0.0d0;
    hold := GetCtrHold(self.s, self.ctr);
    clkSeq := self.c.getSeq(Scalar);
  BEGIN
    res.addhi(Tran.T { 0.0d0, 0.0d0, self.s.riseFall });
    FOR i := 0 TO clkSeq.size()-1 DO
      WITH tran = clkSeq.get(i) DO
        IF tran.v = self.c.hi THEN (* pos edge *)
          IF w # v THEN
            res.addhi(Tran.T { t := tran.t + hold, v := w, rf := self.s.riseFall });
            v := w
          END;
          
          WITH x = self.s.rand.integer(0,1) DO
            CASE x OF 0 => w := 0.0d0 | 1 => w := Vdd ELSE <*ASSERT FALSE*> END
          END
        END
      END
    END;
    RETURN res
  END ArbMakeSeq;

PROCEDURE SeqMakeSeq(self                   : SeqSrc; 
                     READONLY idx : Dims.T) : MemoTranSeq.T =
  VAR
    res := NEW(MemoTranSeq.T).init();
    v := LAST(LONGREAL);
    w := 0.0d0;
    hold := GetCtrHold(self.s,self.ctr);
    clkSeq := self.c.getSeq(Scalar);
    bit : CARDINAL;
    cycle := 0;
  BEGIN
    <*ASSERT NUMBER(idx) <= 1*>
    CASE NUMBER(idx) OF
      0 => bit := 0
    |
      1 => bit := idx[0]
    ELSE
      <*ASSERT FALSE*>
    END;
    Debug.Out(F("SeqMakeSeq : %s%s", self.nodes.nm, Dims.Format(idx)));
    res.addhi(Tran.T { 0.0d0, 0.0d0, self.s.riseFall });
    FOR i := 0 TO clkSeq.size()-1 DO
      WITH tran = clkSeq.get(i) DO
        IF tran.v = self.c.hi THEN (* pos edge of clock *)
          IF w # v THEN
            res.addhi(Tran.T { t := tran.t + hold, v := w, rf := self.s.riseFall });
            Debug.Out(F("SeqMakeSeq: tran t=%s v=%s rf=%s",
                      LR(tran.t+hold), LR(w), LR(self.s.riseFall)));
            v := w
          END;
          
          WITH x = NARROW(self.q[MIN(cycle,LAST(self.q^))],
                                          BitInteger.Concrete).bits[bit] DO
            CASE x OF 0 => w := 0.0d0 | 1 => w := Vdd END
          END;
          INC(cycle)
        END
      END
    END;
    RETURN res
  END SeqMakeSeq;

EXCEPTION Done;

PROCEDURE RMMakeSeq(self         : RMutexSrc;
                    READONLY idx : Dims.T) : MemoTranSeq.T =
  (* fills in all the seqs. in the ring! *)

  PROCEDURE Add(to : CARDINAL; v, t : LONGREAL) : CARDINAL =
    (* returns required hold *)
    BEGIN
      WITH seq = NARROW(seqs.get(to), MemoTranSeq.T),
           src = NARROW(srcs.get(to), RMutexSrc) DO
        seq.addhi(Tran.T { t := t + GetCtrHold(NARROW(self,Src.T).s,src.ctr),
                           v := v,
                           rf := NARROW(self,Src.T).s.riseFall });
        RETURN src.d
      END
    END Add;

  PROCEDURE GetClk() : Tran.T RAISES { Done } =
    BEGIN
      IF c = clkSeq.size() THEN RAISE Done END;
      WITH tr = clkSeq.get(c) DO
        INC(c);
        RETURN tr
      END
    END GetClk; 
            
  VAR 
    c := 0;
    p := self.s.ring.nxt;
    srcs := NEW(RefSeq.T).init();
    clkSeq := self.c.getSeq(Scalar);
    seqs := NEW(RefSeq.T).init();
    prevGrant, sustain, me := LAST(CARDINAL); (* crap value for assertion *)
  BEGIN
    <*ASSERT idx = Scalar*>
    WHILE p # self.s.ring (*not sentinel again*) DO
      IF p.src = self THEN me := srcs.size() END;
      srcs.addhi(p.src);
      WITH seq = NEW(MemoTranSeq.T).init() DO
        seqs.addhi(seq);
        seq.addhi(Tran.T { t := 0.0d0, v := 0.0d0, rf := NARROW(self,Src.T).s.riseFall })
      END;
      p := p.nxt
    END;

    prevGrant := srcs.size();
    TRY
      LOOP
        WITH tran = GetClk() DO
          IF tran.v = self.c.hi THEN (* pos edge *)
            WITH grant = NARROW(self,Src.T).s.rand.integer(0, srcs.size()) DO
              IF grant = srcs.size() THEN
                (* skip *)
              ELSIF grant # prevGrant THEN
                IF prevGrant # srcs.size() THEN
                  EVAL Add(prevGrant, 0.0d0, tran.t)
                END;
                IF grant # srcs.size() THEN
                 sustain := Add(grant    , Vdd  , tran.t)
                END
              END;
              prevGrant := grant;
              
              FOR i := 1 TO sustain-1 DO EVAL GetClk() END 
              (* wait out sustain cycles *)
            END
          END
        END
      END
    EXCEPT
      Done => (* skip *)
    END;

    (* surreptitiously fill in all the others too *)
    FOR i := 0 TO srcs.size()-1 DO
      WITH src = NARROW(srcs.get(i),RMutexSrc) DO
        IF src.trans = NIL THEN
          src.trans := NEW(DimsTranSeqTbl.Default).init()
        END;
        
        WITH hadIt = src.trans.put(Scalar, seqs.get(i)) DO 
          <*ASSERT NOT hadIt*> 
        END
      END
    END;

    RETURN seqs.get(me)

  END RMMakeSeq;


<*UNUSED*>
PROCEDURE NewSemSrcSentinel() : SemSrc =
  BEGIN 
    WITH s = NEW(SemSrc, src := NIL) DO
      s.nxt := s;
      RETURN s
    END
  END NewSemSrcSentinel;

PROCEDURE CompleteRMuSrc(s : RMutexSrc; nds : Nodes.T) =
  BEGIN
    Intf.T.complete(s, nds);
    WITH rec = NEW(SemSrc, src := s, nxt := s.s.ring.nxt) DO
      s.s.ring.nxt := rec
    END
  END CompleteRMuSrc;


PROCEDURE CompleteIntIntf(ii : IntegerSrc; nds : Nodes.T) =
  BEGIN
    Intf.T.complete(ii, nds);
    TYPECASE ii.val OF
      BitInteger.SmallPromise(ism) =>
      CASE NUMBER(nds.dims^) OF
        1 => ii.val := ism.force(nds.dims[0])
      |
        0 => ii.val := ism.force(1)
      ELSE
        Debug.Error("Unsupported multidimensional integer source")
      END
    ELSE
      (* skip *)
    END
  END CompleteIntIntf;

PROCEDURE CompleteSeqSrc(ii : SeqSrc; nds : Nodes.T) =
  BEGIN
    Intf.T.complete(ii, nds);
    FOR i := FIRST(ii.q^) TO LAST(ii.q^) DO
      TYPECASE ii.q[i] OF
        BitInteger.SmallPromise(ism) =>
        CASE NUMBER(nds.dims^) OF
          1 => ii.q[i] := ism.force(nds.dims[0])
        |
          0 => ii.q[i] := ism.force(1)
        ELSE
          Debug.Error("Unsupported multidimensional integer source")
        END
      ELSE
        (* skip *)
      END
    END
  END CompleteSeqSrc;

PROCEDURE StrSeq(str : TEXT) : TextSeq.T =
  VAR
    res := NEW(TextSeq.T).init();
    r := NEW(TextReader.T).init(str);
    w : TEXT;
  CONST
    WS = " \t\n\r";
  BEGIN
    WHILE r.next(WS, w, skipNulls := TRUE) DO
      res.addhi(w)
    END;
    RETURN res
  END StrSeq;
  
BEGIN END SimSupport.
