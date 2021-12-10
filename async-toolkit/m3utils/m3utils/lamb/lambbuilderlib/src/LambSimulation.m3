MODULE LambSimulation;
IMPORT LambVerb AS Verb, Math;
FROM LambSequencer IMPORT Compile;
IMPORT Dims;
IMPORT BitInteger;
FROM Fmt IMPORT F, Int; IMPORT Fmt;
IMPORT LambCommandSeq AS CommandSeq;
IMPORT Random;
IMPORT SimDumper;
FROM SimDumper IMPORT AddNodes, Vdd;
IMPORT Lamb;
IMPORT LambModel;
FROM Dims IMPORT Scalar;
IMPORT SimModel;
IMPORT ParseParams;
IMPORT SimParams;
IMPORT Sim;
IMPORT StandardSettings;
FROM SimSupport IMPORT ClockSrc, IntegerSrc, SeqSrc, Reader;
IMPORT LambPrograms;

CONST SI = BitInteger.Small;

PROCEDURE Clog2(q : CARDINAL) : CARDINAL =
  BEGIN RETURN CEILING(Math.log(FLOAT(q,LONGREAL))/Math.log(2.0d0)) END Clog2;

VAR
  C := Lamb.T {
    N  :=           16,
    W  :=           30,
    LN := LAST(CARDINAL)
  };
    
  I0   := SI( 0);
  I1   := SI( 1);
  I2   := SI( 2);
  Icf9 := SI(16_cf9);
  IN1  := SI(-1);

CONST DutName = "X1";

VAR theModel : SimModel.T;

PROCEDURE Build(pp : ParseParams.T; sp : SimParams.T; modelName : TEXT)
  RAISES { ParseParams.Error }=
  TYPE
    V   = Verb.T;

  PROCEDURE RunTheProgram(pt : LambPrograms.Type) =
    BEGIN
      LambPrograms.Progs[pt](prog,C)
    END RunTheProgram;

  VAR    
    Seq : ARRAY Verb.T OF REF ARRAY OF BitInteger.T;
    prog := NEW(CommandSeq.T).init();
    s := StandardSettings.New(pp, sp, NEW(Random.Default).init());
    theClock := NEW(ClockSrc,
                    s   := s,
                    spd := s.spd,
                    lo  := 0.0d0,
                    hi  := Vdd);
    pt := LambPrograms.ParseFlag(pp);

  BEGIN 
    <*ASSERT s.spd > 0.0d0*>

    C.LN := Clog2( C.N );

    RunTheProgram(pt);

    Compile(prog, Seq);

    SimDumper.SetDutName(DutName);
    AddNodes(ClockName  , Scalar        , theClock                        );
    AddNodes("VDD"      , Scalar        , NEW(IntegerSrc, s := s, val := I1     ) );
    AddNodes("VSS"      , Scalar        , NEW(IntegerSrc, s := s, val := I0     ) );
    
    AddNodes("RADR"     , Dims.T { C.LN } , NEW(SeqSrc    , s := s, q := Seq[V.Radr]
                                                        , c := theClock ) );
    AddNodes("WADR"     , Dims.T { C.LN } , NEW(SeqSrc    , s := s, q := Seq[V.Wadr]
                                                        , c := theClock ) );
    AddNodes("WDATA"     , Dims.T {  C.W } , NEW(SeqSrc    , s := s, q := Seq[V.Wdata]
                                                        , c := theClock ) );
    AddNodes("REN"      , Scalar        , NEW(SeqSrc    , s := s, q := Seq[V.Read]
                                                        , c := theClock ) );
    AddNodes("WEN"      , Scalar        , NEW(SeqSrc    , s := s, q := Seq[V.Writ]
                                                        , c := theClock ) );

    AddNodes("DOUT", Dims.T {  C.W } , NEW(Reader    , c := theClock ) );

    (* dft junk follows *)
    CONST
      DftInNodes = ARRAY OF TEXT { "TEST__SCAN_EN",
                                   "DFT__CORE_SI",
                                   "ICG_FORCE_ON",
                                   "DFT_READ_BYPASS",
                                   "DFT__MEM_WR_DISABLE" };
    BEGIN
      FOR i := FIRST(DftInNodes) TO LAST(DftInNodes) DO
        AddNodes(DftInNodes[i], Scalar, NEW(IntegerSrc, s := s, val := I0     ) )
      END
    END;

    AddNodes("DFT__CORE_SO", Scalar, NEW(Reader, c := theClock));

    (* the module itself *)

    SimDumper.DeclSequence(s.extractPath,
                           F("cdp_lamb_1w1sr_%sw_%sb", Int(C.N), Int(C.W)),
                           ARRAY OF TEXT { "DOUT",
                                           "DFT__CORE_SO",
                                           "RADR",
                                           "REN",
                                           "WDATA",
                                           "WADR",
                                           "WEN",
                                           "TEST__SCAN_EN",
                                           "DFT__CORE_SI",
                                           "ICG_FORCE_ON",
                                           "DFT_READ_BYPASS",
                                           "DFT__MEM_WR_DISABLE",
                                           "CLK",
                                           "VSS",
                                           "VDD" },
                           reverseArrays := TRUE (* some weird BFF thing *)
    );


  theModel := NEW(LambModel.T).init(C,
                                    s.cycleTime,
                                    s.assertHoldFrac,
                                    s.assertHoldTime)
    
END Build;

PROCEDURE GetModel() : SimModel.T = 
  BEGIN RETURN theModel END GetModel;

(**********************************************************************)

BEGIN END LambSimulation.
