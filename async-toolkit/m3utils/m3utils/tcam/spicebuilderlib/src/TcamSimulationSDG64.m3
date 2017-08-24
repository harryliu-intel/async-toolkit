MODULE TcamSimulationSDG64;
IMPORT Verb, Math;
FROM TcamSequencer IMPORT Compile;
IMPORT Dims;
IMPORT BitInteger;
FROM Fmt IMPORT F, Int; IMPORT Fmt;
IMPORT CommandSeq;
IMPORT Random;
IMPORT SimDumper;
FROM SimDumper IMPORT AddNodes, Vdd;
IMPORT Tcam;
IMPORT TcamModel;
FROM Dims IMPORT Scalar;
IMPORT SimModel;
IMPORT ParseParams;
IMPORT SimParams;
IMPORT Sim;
IMPORT StandardSettings;
FROM SimSupport IMPORT ClockSrc, IntegerSrc, SeqSrc, Reader;
IMPORT TcamPrograms;
IMPORT TcamIONodes;
IMPORT Bundle;
IMPORT SimSupport;
IMPORT Scan;
IMPORT Variation_p1274_3x2r1 AS TheVariation;
(*IMPORT Variation_p1274_0x1r1 AS TheVariation;*)
IMPORT Variation; FROM Variation IMPORT SetP;
IMPORT HspVariation;
IMPORT Wr;
IMPORT TextWr;
IMPORT FloatMode, Lex;
IMPORT Debug;
IMPORT SimMeasurement;
IMPORT Text;
IMPORT TextSeq, IntSeq;

TYPE TA = ARRAY OF TEXT;
     LA = ARRAY OF LONGREAL;
     
CONST SI = BitInteger.Small;
CONST TE = Text.Equal;

PROCEDURE Clog2(q : CARDINAL) : CARDINAL =
  BEGIN RETURN CEILING(Math.log(FLOAT(q,LONGREAL))/Math.log(2.0d0)) END Clog2;

VAR
  C := Tcam.T {
    N  :=           64,
    W  :=           42,
    LN := LAST(CARDINAL),
    CN :=           12,
    SS :=           64 (* depth of a slice *),
    SN := LAST(CARDINAL)
  };
    
  I0   := SI( 0);
  I1   := SI( 1);
  cfgVal := SI(16_cf9);
  IN1  := SI(-1);

CONST DutName = "X1";

VAR theModel : SimModel.T;

TYPE
  VariationDeck = { Stability, HitLeakage };

CONST
  VariationDeckNames = ARRAY VariationDeck OF TEXT { "stability", "hit_leakage" };
  PNms   = TA {   "M", "Wdrawn", "Ldrawn",  "NF" };
(*
Mqn1 b[0] a[0] x0[0] vss nsvt_var00 L=0.02u W=0.034u 
Mqn13 x0[1] x0[0] vss vss nsvt_var01 L=0.02u W=0.034u 
Mqn15 x0[0] x0[1] vss vss nsvt_var02 L=0.02u W=0.034u 
Mqn2 b[1] a[0] x0[1] vss nsvt_var03 L=0.02u W=0.034u 
Mqp2 x0[1] x0[0] vcc vcc psvt_var04 L=0.02u W=0.034u 
Mqp8 x0[0] x0[1] vcc vcc psvt_var05 L=0.02u W=0.034u 
Mqn5 hit[0] x0[0] h0_l vss nsvt_var06 L=0.02u W=0.102u 
*)
  MPStability = ARRAY OF LA {
           LA { 1.0d0,    34.0d-9,    20.0d-9, 1.0d0 },
           LA { 1.0d0,    34.0d-9,    20.0d-9, 1.0d0 },
           LA { 1.0d0,    34.0d-9,    20.0d-9, 1.0d0 },
           LA { 1.0d0,    34.0d-9,    20.0d-9, 1.0d0 },
           LA { 1.0d0,    34.0d-9,    20.0d-9, 1.0d0 },
           LA { 1.0d0,    34.0d-9,    20.0d-9, 1.0d0 },
           LA { 1.0d0,   102.0d-9,    20.0d-9, 1.0d0 }
    };
  MtStability   = TA { "nsvt", "nsvt", "nsvt", "nsvt", "psvt", "psvt", "nsvt" };
(*
Mqn0 h0_l k[0] vss vss nsvt_var00 L=0.02u W=0.102u 
Mqn5 hit[0] x0[0] h0_l vss nsvt_var01 L=0.02u W=0.102u 
Mqn7 ho_r k[1] vss vss nsvt_var00 L=0.02u W=0.102u 
Mqn8 hit[0] x1[0] ho_r vss nsvt_var01 L=0.02u W=0.102u 
*)
  MPHitLeakage = ARRAY OF LA {
           LA { 1.0d0,   102.0d-9,    20.0d-9, 1.0d0 },
           LA { 1.0d0,   102.0d-9,    20.0d-9, 1.0d0 }
};
  MtHitLeakage   = TA { "nsvt", "nsvt", "nsvt", "nsvt" };

PROCEDURE CopyMP(READONLY a : ARRAY OF LA) : REF ARRAY OF LA =
  BEGIN
    WITH res = NEW(REF ARRAY OF LA, NUMBER(a), NUMBER(a[0])) DO
      res^ := a;
      RETURN res
    END
  END CopyMP;
  
PROCEDURE GetMP(nm : TEXT) : REF ARRAY OF LA =
  BEGIN
    IF    TE(nm, VariationDeckNames[VariationDeck.Stability]) THEN
      RETURN CopyMP(MPStability)
    ELSIF TE(nm, VariationDeckNames[VariationDeck.HitLeakage]) THEN
      RETURN CopyMP(MPHitLeakage)
    ELSE
      Debug.Error("Unknown variation deck name \"" & nm & "\"");
      <*ASSERT FALSE*>
    END
  END GetMP;

PROCEDURE CopyMt(READONLY a : TA) : REF TA =
  BEGIN
    WITH res = NEW(REF TA, NUMBER(a)) DO
      res^ := a;
      RETURN res
    END
  END CopyMt;

PROCEDURE GetMt(nm : TEXT) : REF TA =
  BEGIN
    IF    TE(nm, VariationDeckNames[VariationDeck.Stability]) THEN
      RETURN CopyMt(MtStability)
    ELSIF TE(nm, VariationDeckNames[VariationDeck.HitLeakage]) THEN
      RETURN CopyMt(MtHitLeakage)
    ELSE
      Debug.Error("Unknown variation deck name \"" & nm & "\"");
      <*ASSERT FALSE*>
    END
  END GetMt;

  (**********************************************************************)

PROCEDURE CountI(seq : IntSeq.T; q : INTEGER) : CARDINAL =
  VAR
    res := 0;
  BEGIN
    FOR i := 0 TO seq.size()-1 DO
      IF seq.get(i) = q THEN INC(res) END
    END;
    RETURN res
  END CountI;
      
PROCEDURE DoVariation(wr                : Wr.T;
                      READONLY MV       : ARRAY OF LA;
                      READONLY Mt       : TA;
                      READONLY vars     : ARRAY OF LONGREAL;
                      varNameSeq        : TextSeq.T;
                      tranIdSeq         : IntSeq.T;
                      modelName         : TEXT;
                      theVariation      : Variation.T)
  RAISES { Wr.Failure } =
  BEGIN
    (* count params *)
    FOR i := FIRST(MV) TO LAST(MV) DO
      VAR
        nGeoParams := NUMBER(MV[i]);
        nVarParams := CountI(tranIdSeq, i);
        np         := nGeoParams + nVarParams;
        params     := NEW(REF ARRAY OF Variation.Param, np);
        k          := 0;
      BEGIN
        Debug.Out(F("DoVariation : nGeoParams %s nVarParams %s : np %s",
                    Int(nGeoParams), Int(nVarParams), Int(np)));
        FOR j := 0 TO nGeoParams-1 DO
          Debug.Out(F("DoVariation geo param[%s] %s", Int(k), PNms[j]));
          params[k].nm := PNms[j];
          params[k].v  := MV[i][j];
          INC(k)
        END;
        FOR u := FIRST(vars) TO LAST(vars) DO
          IF tranIdSeq.get(u) = i THEN
            Debug.Out(F("DoVariation var param[%s] %s", Int(k), varNameSeq.get(u)));
            params[k].nm := varNameSeq.get(u);
            params[k].v  := vars[u];
            INC(k)
          END
        END;
        <*ASSERT k = np*>

        theVariation.wrModel(wr,
                             fromLib          := modelName,
                             baseTranTypeName := Mt[i],
                             newTranTypeName  := F("%s_var%02s",
                                                   Mt[i],
                                                   Fmt.Int(i)),
                             params           := params^);
      END
    END
  END DoVariation;

  (**********************************************************************)

PROCEDURE FormatVar(vn : TEXT; ti : INTEGER) : TEXT =
  BEGIN
    RETURN Fmt.Int(ti) & ":" & vn
  END FormatVar;
  
PROCEDURE ParseAndDoVariation(pp : ParseParams.T; modelName : TEXT) =
        <*FATAL Wr.Failure*>
  VAR
    deckName     := pp.getNext();
    mp           := GetMP(deckName); (* geometric parameters *)
    mt           := GetMt(deckName); (* transistor type names *)
    vars         : REF ARRAY OF LONGREAL;
    wr           := TextWr.New();
    theVariation : HspVariation.T := GetVariation();
    varNameSeq   := NEW(TextSeq.T).init();
    tranIdSeq    := NEW(IntSeq.T).init();
  BEGIN
    FOR i := FIRST(TheVariation.ConstParams) TO LAST(TheVariation.ConstParams) DO
      theVariation.setGlobalParam(TheVariation.ConstParams[i].nm,
                                  TheVariation.ConstParams[i].v)
    END;
    FOR i := FIRST(mt^) TO LAST(mt^) DO
      WITH vars = theVariation.getVars(modelName, mt[i])^ DO
        (* unpack every variation *)
        FOR j := FIRST(vars) TO LAST(vars) DO
          Debug.Out("Building variations... " & FormatVar(vars[j], i));
          varNameSeq.addhi(vars[j]);
          tranIdSeq.addhi(i)
        END
      END
    END;

    vars := NEW(REF ARRAY OF LONGREAL, varNameSeq.size());
    
    FOR i := FIRST(vars^) TO LAST(vars^) DO
      WITH nm = pp.getNext() DO
        IF NOT TE(nm, FormatVar(varNameSeq.get(i), tranIdSeq.get(i))) THEN
          Debug.Error(F("TcamSimulationSDG64.ParseAndDoVariation : command line argument in wrong sequence, expected \"%s\" but got \"%s\"",
                       FormatVar(varNameSeq.get(i), tranIdSeq.get(i)), nm ))
        END;
        vars[i] := pp.getNextLongReal()
      END
    END;
    DoVariation(wr, mp^, mt^, vars^, varNameSeq, tranIdSeq, modelName,
                theVariation);
    SimDumper.SetVarModels(TextWr.ToText(wr))
  END ParseAndDoVariation;
  
PROCEDURE Build(pp : ParseParams.T; sp : SimParams.T; modelName : TEXT)
  RAISES { ParseParams.Error }=
  TYPE
    V   = Verb.T;

  PROCEDURE RunTheProgram(pt : TcamPrograms.Type) =
    BEGIN
      TcamPrograms.Progs[pt](prog,C)
    END RunTheProgram;

  VAR    
    Seq : ARRAY Verb.T OF REF ARRAY OF BitInteger.T;
    prog := NEW(CommandSeq.T).init();
    s := StandardSettings.New(pp, sp, NEW(Random.Default).init());
    theClock := NEW(ClockSrc,
                    s := s,
                    spd := s.spd,
                    lo := 0.0d0,
                    hi := Vdd);
    pt := TcamPrograms.ParseFlag(pp);

  BEGIN

    Debug.Out("Model name is " & modelName);
    IF pp.keywordPresent("-cfg") THEN
      TRY
        cfgVal := SI(Scan.Unsigned(pp.getNext()))
      EXCEPT
        FloatMode.Trap, Lex.Error => Debug.Error("Illegal format for -cfg")
      END
    END;
    C.LN := Clog2( C.N*2 );
    C.SN := C.N DIV C.SS;

    RunTheProgram(pt);

    Compile(prog, Seq);

    IF pp.keywordPresent("-var") THEN
      ParseAndDoVariation(pp, modelName)
    END;

    SimDumper.SetDutName(DutName);
    AddNodes(ClockName  , Scalar        , theClock                        );
    AddNodes("vcc"      , Scalar        , NEW(IntegerSrc, s := s, val := I1     ) );
    (*
    AddNodes("vss"      , Scalar        , NEW(IntegerSrc, s := s, val := I0     ) ); (* not in SDG TCAM ? *)
    *)
    AddNodes("vccu"      , Scalar        , NEW(IntegerSrc, s := s, val := I1     ) ); (* not sure about this *)
    
    
    AddNodes("cfg"      , Dims.T { C.CN } , NEW(IntegerSrc, s := s, val := cfgVal   ) );

    AddNodes("addr"     , Dims.T { C.LN } , NEW(SeqSrc    , s := s, q := Seq[V.Addr]
                                                        , c := theClock ) );
    AddNodes("data"     , Dims.T {  C.W } , NEW(SeqSrc    , s := s, q := Seq[V.Data]
                                                        , c := theClock ) );
    AddNodes("ken"      , Scalar        , NEW(SeqSrc    , s := s, q := Seq[V.Look]
                                                        , c := theClock ) );
    AddNodes("ren"      , Scalar        , NEW(SeqSrc    , s := s, q := Seq[V.Read]
                                                        , c := theClock ) );
    AddNodes("wen"      , Scalar        , NEW(SeqSrc    , s := s, q := Seq[V.Writ]
                                                        , c := theClock ) );

    AddNodes("read_data", Dims.T {  C.W } , NEW(Reader    , c := theClock ) );
    AddNodes("rhit"     , Dims.T {  C.N } , NEW(Reader    , c := theClock ) );

    AddNodes("mask"     , Dims.T {  C.W } , NEW(IntegerSrc, s := s, val := IN1    ) );
    AddNodes("lhit"     , Dims.T {  C.N } , NEW(IntegerSrc, s := s, val := IN1    ) );

    AddNodes("opwrenoutb", Scalar, NEW(Reader, c := theClock) );
    AddNodes("fscan_sin", Scalar, NEW(IntegerSrc, s := s, val := I0) );
    AddNodes("fscan_shiften", Scalar, NEW(IntegerSrc, s := s, val := I0) );
    AddNodes("ipwreninb", Scalar, NEW(IntegerSrc, s := s, val := I0) );
    AddNodes("fscan_sout", Scalar, NEW(Reader, c := theClock) );

    IF C.SN = 1 THEN
      AddNodes("slice_en" , Scalar , NEW(IntegerSrc, s := s, val := IN1    ) );
    ELSE
      AddNodes("slice_en" , Dims.T { C.SN } , NEW(IntegerSrc, s := s, val := IN1    ) );
    END;
    AddNodes("reset_n"  , Scalar        , NEW(SeqSrc    , s := s, q := Seq[V.Rset]
                                                        , c := theClock ) );

    SimDumper.DeclSequence(s.extractPath,
                           "s742rf040b064e1r1w1tbehsaa4pc2",
                           ARRAY OF TEXT { "addr", 
                                           "cfg",
                                           ClockName,
                                           "data",
                                           "ken",
                                           "lhit",
                                           "mask",
                                           "read_data",
                                           "ren",
                                           "reset_n",
                                           "rhit",
                                           "slice_en",
                                           "vcc",
                                           "vccu", (* ??? *)
                                           "wen",
                                           "opwrenoutb",
                                           "fscan_sin",
                                           "fscan_sout",
                                           "fscan_shiften",
                                           "ipwreninb" });
    
    WITH bundle = TcamIONodes.Get(),
         str    = Bundle.Get(bundle, "sdg_tcam64_42_nodelist"),
         seq    = SimSupport.StrSeq(str) DO
      SimDumper.SetInterfaceNodeSequence(seq)
    END;
    SimDumper.simExtras[Sim.T.XA].addhi("V999999 vss 0");
    SimDumper.AddProbes(SimDumper.ProbeType.Current, "vcc");

    IF s.verboseNodes THEN
      (* debugging goes here *)
      SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"probe_waveform_voltage %s* -limit 10\"", SimDumper.Renamer(DutName & ".")));
    END;
    
    theModel := NEW(TcamModel.T).init(C,
                                      s.cycleTime,
                                      s.assertHoldFrac,
                                      s.assertHoldTime,
                                      TcamModel.NamingConvention.SDG);

    IF pp.keywordPresent("-measure") THEN
      WITH mm = pp.getNext() DO
        IF TE(mm, "bitlinebump") THEN
          SimDumper.AddMeasurement(
              NEW(SimMeasurement.Default,
                  nodeNm := "Xislice64_40.Xchunk_btm.Xarr[0].Xtbit4[0].x0[0]",
                  type := SimMeasurement.Type.Max,
                  from := NEW(SimMeasurement.Trigger,
                              spec := NEW(SimMeasurement.Clock),
                              op   := SimMeasurement.Op.Up,
                              val  := 8.0d0),
                  to   := NEW(SimMeasurement.Trigger,
                              spec := NEW(SimMeasurement.Clock),
                              op   := SimMeasurement.Op.Up,
                              val  := 8.5d0)))
        ELSIF TE(mm, "hitlinedroop") THEN
          WITH startTrigger = NEW(SimMeasurement.Trigger,
                              spec := NEW(SimMeasurement.Clock),
                              op   := SimMeasurement.Op.Up,
                              val  := 5.0d0),
               stopTrigger  = NEW(SimMeasurement.Trigger,
                                  spec := NEW(SimMeasurement.Default,
                                              nodeNm := "Xislice64_40.Xxmid[0].Xout_hit[0].l1",
                                              type := SimMeasurement.Type.Last,
                                              from := startTrigger,
                                              to   := NIL),
                                  op := SimMeasurement.Op.Up,
                                  val := Vdd * 0.9d0),
               theMeasurement = NEW(SimMeasurement.Default,
                                    nodeNm := "Xislice64_40.net62[0]",
                                    type := SimMeasurement.Type.Min,
                                    from := startTrigger,
                                    to   := stopTrigger) DO
            SimDumper.AddMeasurement(theMeasurement)
          END
        ELSE
          Debug.Error(F("Unknown measurement \"%s\"",mm))
        END
      END
    END
  END Build;

PROCEDURE GetModel() : SimModel.T =
  BEGIN RETURN theModel END GetModel;

(**********************************************************************)

PROCEDURE GetVarNames(varScenarioName, modelName : TEXT) : REF ARRAY OF TEXT =
  VAR
    variation : HspVariation.T := GetVariation();
    mt := GetMt(varScenarioName);
    seq := NEW(TextSeq.T).init();
  BEGIN
    FOR i := FIRST(mt^) TO LAST(mt^) DO
      WITH vars = variation.getVars(modelName, mt[i])^ DO
        FOR j := FIRST(vars) TO LAST(vars) DO
          seq.addhi(FormatVar(vars[j], i))
        END
      END
    END;
    WITH res = NEW(REF ARRAY OF TEXT, seq.size()) DO
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := seq.get(i)
      END;
      RETURN res
    END
  END GetVarNames;

VAR hspVarMu := NEW(MUTEX);
    hspVariation : HspVariation.T := NIL;

PROCEDURE GetVariation() : HspVariation.T =
  BEGIN
    LOCK hspVarMu DO
      IF hspVariation = NIL THEN 
        hspVariation := NEW(HspVariation.T).init(TheVariation.HspName)
      END;
      RETURN hspVariation
    END
  END GetVariation;
  
BEGIN END TcamSimulationSDG64.
