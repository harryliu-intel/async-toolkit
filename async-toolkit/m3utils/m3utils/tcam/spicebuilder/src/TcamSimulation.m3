MODULE TcamSimulation;
IMPORT Command, Verb, Math;
FROM TcamSequencer IMPORT Compile, AddKey;
IMPORT Dims;
IMPORT BitInteger;
IMPORT MemoTranSeq;
IMPORT Src;
IMPORT Intf;
IMPORT Nodes, Debug;
IMPORT Tran;
FROM Fmt IMPORT F; IMPORT Fmt;
IMPORT RefSeq;
IMPORT CommandSeq;
IMPORT DimsTranSeqTbl;
IMPORT Random;
IMPORT SimDumper;
FROM SimDumper IMPORT AddNodes, Vdd;
IMPORT Rdr;
IMPORT Tcam;
IMPORT TcamModel;
FROM Dims IMPORT Scalar;
IMPORT SimModel;
IMPORT ParseParams;
IMPORT SimParams;
IMPORT Sim;
IMPORT Text;

CONST TE = Text.Equal;

CONST LR = Fmt.LongReal;

(*
VAR
  sem := NEW(Semaphore, ring := NewSemSrcSentinel());
*)

TYPE
  IntegerSrc = Src.T BRANDED OBJECT 
    val : BitInteger.T; 
  OVERRIDES
    complete := CompleteIntIntf;
    makeSeq  := IntegerMakeSeq;
  END;
  
  ClockedSrcIntf = Src.T BRANDED OBJECT
    ctr : REF Constraints := NIL;
    c   : ClockSrc;
  END;

  RMutexSrc  = ClockedSrcIntf BRANDED OBJECT 
    s : Semaphore; 
    d : CARDINAL; 
    mutexVal := TRUE 
  OVERRIDES
    complete := CompleteRMuSrc;
    makeSeq  := RMMakeSeq;
  END;

  ArbSrc  = ClockedSrcIntf BRANDED OBJECT 
  OVERRIDES
    makeSeq := ArbMakeSeq;
  END;

  SeqSrc  = ClockedSrcIntf BRANDED OBJECT 
    q : REF ARRAY OF BitInteger.T;
  OVERRIDES
    complete := CompleteSeqSrc;
    makeSeq := SeqMakeSeq;
  END;

  ClockedRdrIntf = Rdr.T BRANDED OBJECT
    ctr : Constraints;
    c   : ClockSrc;
  END;

  Constraints = RECORD
    setup : LONGREAL;
    hold  : LONGREAL;
  END;

  ClockSrc   = Src.T BRANDED OBJECT 
    lo, hi : LONGREAL;
    spd    : LONGREAL; 
  OVERRIDES
    makeSeq := ClockMakeSeq;
    infinite := CSInfinite;
  END;
  
  Reader     = ClockedRdrIntf BRANDED OBJECT END;

  Semaphore  = OBJECT
    ring : SemSrc := NIL;
  END;

  SemSrc = OBJECT
    src : Src.T;
    nxt : SemSrc;
  END;

VAR defConstraints : Constraints;

PROCEDURE GetCtrHold(x : REF Constraints) : LONGREAL =
  BEGIN
    IF x = NIL THEN RETURN defConstraints.hold ELSE RETURN x.hold END
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
    res.addhi(Tran.T { t := 0.0d0, v := v, rf := riseFall });
    RETURN res
  END IntegerMakeSeq;

PROCEDURE ClockMakeSeq(self : ClockSrc; READONLY idx : Dims.T) : MemoTranSeq.T =
  VAR
    res := NEW(MemoTranSeq.T).init();
    v := self.lo;
    t := 0.0d0;
  BEGIN
    <*ASSERT idx = Scalar*>
    WHILE t < simParams.maxTime DO
      res.addhi(Tran.T { t := t, v := v, rf := riseFall });
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
    hold := GetCtrHold(self.ctr);
    clkSeq := self.c.getSeq(Scalar);
  BEGIN
    res.addhi(Tran.T { 0.0d0, 0.0d0, riseFall });
    FOR i := 0 TO clkSeq.size()-1 DO
      WITH tran = clkSeq.get(i) DO
        IF tran.v = self.c.hi THEN (* pos edge *)
          IF w # v THEN
            res.addhi(Tran.T { t := tran.t + hold, v := w, rf := riseFall });
            v := w
          END;
          
          WITH x = rand.integer(0,1) DO
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
    hold := GetCtrHold(self.ctr);
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
    res.addhi(Tran.T { 0.0d0, 0.0d0, riseFall });
    FOR i := 0 TO clkSeq.size()-1 DO
      WITH tran = clkSeq.get(i) DO
        IF tran.v = self.c.hi THEN (* pos edge of clock *)
          IF w # v THEN
            res.addhi(Tran.T { t := tran.t + hold, v := w, rf := riseFall });
            Debug.Out(F("SeqMakeSeq: tran t=%s v=%s rf=%s",
                      LR(tran.t+hold), LR(w), LR(riseFall)));
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
        seq.addhi(Tran.T { t := t + GetCtrHold(src.ctr), v := v, rf := riseFall });
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
        seq.addhi(Tran.T { t := 0.0d0, v := 0.0d0, rf := riseFall })
      END;
      p := p.nxt
    END;

    prevGrant := srcs.size();
    TRY
      LOOP
        WITH tran = GetClk() DO
          IF tran.v = self.c.hi THEN (* pos edge *)
            WITH grant = rand.integer(0, srcs.size()) DO
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

CONST SI = BitInteger.Small;

PROCEDURE Clog2(q : CARDINAL) : CARDINAL =
  BEGIN RETURN CEILING(Math.log(FLOAT(q,LONGREAL))/Math.log(2.0d0)) END Clog2;

VAR
  theClock : ClockSrc;

VAR
  C := Tcam.T {
    N  :=          512,
    W  :=           40,
    LN := LAST(CARDINAL),
    CN :=           12,
    SS :=           64 (* depth of a slice *),
    SN := LAST(CARDINAL)
  };
    
  I0 := SI( 0);
  I1 := SI( 1);
  IN1:= SI(-1);

CONST DutName = "X1";

VAR simParams : SimParams.T;

PROCEDURE Build(pp : ParseParams.T; sp : SimParams.T) =

  TYPE
    AT = ARRAY OF TEXT;

  PROCEDURE PP(READONLY opts : AT; VAR x : LONGREAL) =
    BEGIN
      FOR i := FIRST(opts) TO LAST(opts) DO
        IF pp.keywordPresent("-" & opts[i]) THEN x := pp.getNextLongReal() END
      END
    END PP;

  TYPE
    V   = Verb.T;
  VAR    
    Seq : ARRAY Verb.T OF REF ARRAY OF BitInteger.T;
    prog := NEW(CommandSeq.T).init();

    spd          := 1.0d9;
    riseFallFrac := 0.02d0;
    setupFrac    := 0.000d0;
    holdFrac     := 0.150d0;
    cyc : LONGREAL;
    pt := ProgType.Def;

  PROCEDURE RunTheProgram(pt : ProgType) =
    BEGIN
      Progs[pt](prog)
    END RunTheProgram;

  BEGIN 
    simParams := sp;

    PP(AT { "clk" }                     , spd);
    PP(AT { "holdfrac" }                , holdFrac);
    PP(AT { "setupfrac" }               , setupFrac);
    PP(AT { "risefrac", "risefallfrac" }, riseFallFrac);
    
    IF pp.keywordPresent("-prog") THEN
      VAR found := FALSE;
          nm := pp.getNext();
      BEGIN
        FOR i := FIRST(ProgType) TO LAST(ProgType) DO
          IF TE(ProgNames[i], nm) THEN
            pt := i; found := TRUE; EXIT
          END
        END;
        IF NOT found THEN Debug.Error("Unknown TCAM program \"" & nm & "\"") END
      END
    END;
    
    cyc      := (1.0d0/spd);

    defConstraints.hold := holdFrac * cyc;
    defConstraints.setup := setupFrac * cyc;
    
    riseFall  := riseFallFrac * cyc;

    theClock := NEW(ClockSrc, spd := spd, lo := 0.0d0, hi := Vdd);
    C.LN := Clog2( C.N*2 );
    C.SN := C.N DIV C.SS;

    RunTheProgram(pt);

    Compile(prog, Seq);

    SimDumper.SetDutName(DutName);
    AddNodes(ClockName  , Scalar        , theClock                        );
    AddNodes("VDD"      , Scalar        , NEW(IntegerSrc, val := I1     ) );
    AddNodes("VSS"      , Scalar        , NEW(IntegerSrc, val := I0     ) );
    
    AddNodes("CFG"      , Dims.T { C.CN } , NEW(IntegerSrc, val := I0     ) );

    AddNodes("ADDR"     , Dims.T { C.LN } , NEW(SeqSrc    , q := Seq[V.Addr]
                                                        , c := theClock ) );
    AddNodes("DATA"     , Dims.T {  C.W } , NEW(SeqSrc    , q := Seq[V.Data]
                                                        , c := theClock ) );
    AddNodes("KEN"      , Scalar        , NEW(SeqSrc    , q := Seq[V.Look]
                                                        , c := theClock ) );
    AddNodes("REN"      , Scalar        , NEW(SeqSrc    , q := Seq[V.Read]
                                                        , c := theClock ) );
    AddNodes("WEN"      , Scalar        , NEW(SeqSrc    , q := Seq[V.Writ]
                                                        , c := theClock ) );

    AddNodes("READ_DATA", Dims.T {  C.W } , NEW(Reader    , c := theClock ) );
    AddNodes("RHIT"     , Dims.T {  C.N } , NEW(Reader    , c := theClock ) );

    AddNodes("MASK"     , Dims.T {  C.W } , NEW(IntegerSrc, val := IN1    ) );
    AddNodes("LHIT"     , Dims.T {  C.N } , NEW(IntegerSrc, val := IN1    ) );

    AddNodes("SLICE_EN" , Dims.T { C.SN } , NEW(IntegerSrc, val := IN1    ) );
    AddNodes("RESET_N"  , Scalar        , NEW(SeqSrc    , q := Seq[V.Rset]
                                                        , c := theClock ) );

  SimDumper.DeclSequence("/p/hlp/thkoo1/lion/tcam/inway7_extraction/ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_512_40_4000_rcx/ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_512_40_4000.sp",
               "ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl",
               ARRAY OF TEXT { "ADDR", 
                               "CFG",
                               ClockName,
                               "DATA",
                               "KEN",
                               "LHIT",
                               "MASK",
                               "READ_DATA",
                               "REN",
                               "RESET_N",
                               "RHIT",
                               "SLICE_EN",
                               "VDD",
                               "VSS",
                               "WEN" });



  SimDumper.simExtras[Sim.T.XA].addhi(".OPTION XA_CMD=\"probe_waveform_voltage X1.* -limit 2\"");


END Build;

PROCEDURE GetModel() : SimModel.T = 
  BEGIN RETURN NEW(TcamModel.T).init(C) END GetModel;

VAR riseFall := LAST(LONGREAL);

VAR rand := NEW(Random.Default).init();


(**********************************************************************)

PROCEDURE DefProgram(prog : CommandSeq.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  0 }); (* 4.5 *)
    AddCmd(  Cmd { V.Writ,            -1,  1 }); (* 6.5 *)
    (* works if no more writes here *)

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Writ, 16_00c0edbabe,  2 });
    AddCmd(  Cmd { V.Writ, 16_00c001d00d,  3 });

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Writ, 16_ffffffff00,  4 }); (* 10.5 *)
    AddCmd(  Cmd { V.Writ,            -1,  5 }); (* 12.5 *)

    AddCmd(  Cmd { V.Look,             0     });

    AddKey(prog, 10,        "0000000000000000000000000000000000000000", C.W); (* 16.5 *)

    AddCmd(  Cmd { V.Look,             0     }); (* 19.5 *)

    AddKey(prog, 11,        "0000000000000000000000000000000000000001", C.W);

    AddCmd(  Cmd { V.Look,             0     });

    AddKey(prog, 11,        "0000000000000000000000000000000000000?11", C.W);

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Read,             0     });
    AddCmd(  Cmd { V.Read,             1     });

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000000     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000011     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000111     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000001111     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000000     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000011     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000111     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000001111     });
  END DefProgram;

(**********************************************************************)

PROCEDURE ShortProgram(prog : CommandSeq.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  0 }); (* 4.5 *)
    AddCmd(  Cmd { V.Writ,            -1,  1 }); (* 6.5 *)
    (* works if no more writes here *)

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Writ, 16_ffffffff00,  4 }); (* 10.5 *)
    AddCmd(  Cmd { V.Writ,            -1,  5 }); (* 12.5 *)

    AddCmd(  Cmd { V.Look,             0     });

    AddKey(prog, 10,        "0000000000000000000000000000000000000000", C.W); (* 16.5 *)

    AddCmd(  Cmd { V.Look,             0     }); (* 19.5 *)

    AddKey(prog, 11,        "0000000000000000000000000000000000000001", C.W);

    AddCmd(  Cmd { V.Look,             0     });

    AddKey(prog, 11,        "0000000000000000000000000000000000000?11", C.W);

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Read,             0     });
    AddCmd(  Cmd { V.Read,             1     });

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000000     })

  END ShortProgram;

(**********************************************************************)

PROCEDURE MinimalProgram(prog : CommandSeq.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    AddCmd(  Cmd { V.Rset                    });
    AddKey(prog,  0,        "0000000000000000000000000000000000000000", C.W);
    AddCmd(  Cmd { V.Look,             0     });
    AddCmd(  Cmd { V.Look,             1     });
    AddCmd(  Cmd { V.Look,             0     });
    AddCmd(  Cmd { V.Look,             1     });
  END MinimalProgram;

(**********************************************************************)

TYPE ProgType = { Def, Short, Minimal };
CONST  ProgNames = ARRAY ProgType OF TEXT { "def", "short", "minimal" };

TYPE ProgProc = PROCEDURE(prog : CommandSeq.T);

CONST Progs = ARRAY ProgType OF ProgProc 
  { DefProgram, ShortProgram, MinimalProgram };

BEGIN END TcamSimulation.
