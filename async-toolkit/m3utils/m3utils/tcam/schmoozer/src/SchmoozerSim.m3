MODULE SchmoozerSim EXPORTS Main;
IMPORT Schmoozer, Schmooze;

IMPORT ProbeMode, Sim;

FROM Schmoozer IMPORT DiscreteParam, RealParam, IntParam;
FROM Schmoozer IMPORT Variety, Sweep, Schmoo, SweepSpecific, SpecificInt, VarOpt;
FROM Schmoozer IMPORT LA, TA, IA, RP01, LR01, RP02, LR02;
FROM Schmoozer IMPORT Tool;
FROM Schmoozer IMPORT Add, RL, RT, RP, RI;

FROM DefParams IMPORT vddP, clkP, tempP, holdP;
FROM Schmoozer IMPORT simP;
IMPORT TcamPrograms;
IMPORT Text;
IMPORT Debug;
IMPORT ParseParams;
FROM Fmt IMPORT F;
IMPORT Stdio;
IMPORT TcamSimulationSDG64;

CONST TE = Text.Equal;
      
VAR
  probeP  := NEW(DiscreteParam,
                 nm := "probemode",
                 flag := "probemode",
                 vals := RT(ProbeMode.Names)).init();

  cornerP := NEW(DiscreteParam,
                 nm := "corner",
                 flag := "corner",
                 vals := RT(TA { 
                                 "afnoif",
                                 "afnois",
                                 "bghlhh",
                                 "bghllh",
                                 "bghlth",
                                 "bglhll",
                                 "bglhll2",
                                 "bglhlt",
                                 "bgllll",
                                 "bgtlhh",
                                 "ffff",
                                 "ffvss",
                                 "pfff",
                                 "psss",
                                 "rafas",
                                 "rasaf",
                                 "rfafs",
                                 "rfafs_gs",
                                 "rfasf",
                                 "rfasf_gs",
                                 "rfff",
                                 "rfffaf",
                                 "rfffaf_gs",
                                 "rffs",
                                 "rffx",
                                 "rffxaf",
                                 "rfsf",
                                 "rfxf",
                                 "rfxfaf",
                                 "rsafs",
                                 "rsafs_gf",
                                 "rsasf",
                                 "rsasf_gf",
                                 "rsfs",
                                 "rssf",
                                 "rsss",
                                 "rsssas",
                                 "rsssas_gf",
                                 "rssx",
                                 "rssxas",
                                 "rsxs",
                                 "rsxsas",
                                 "ssss",
                                 "ssvff",
                                 "tttt"
  })).init();


  spfP    := NEW(RealParam,
                 nm := "spf",
                 flag := "spf",
                 saneMin := 0.0d0, saneMax := 1.0d30).init();

  assertHoldP   := NEW(RealParam,
                 tool := Tool.PostSpice,
                 nm := "assertholdfrac",
                 flag := "assertholdfrac",
                 saneMin := -1.0d0, saneMax := 1.0d0).init();

  assertHoldTimeP   := NEW(RealParam,
                 tool := Tool.PostSpice,
                 nm := "assertholdtime",
                 flag := "assertholdtime",
                 saneMin := -1.0d0, saneMax := 1.0d0).init();

  deckP   := NEW(DiscreteParam,
                 tool := Tool.Script,
                 nm := "deck",
                 flag := "deck",
                 vals := RT(TA { 
                                 "bothrs", (* s.b. default *)
                                 "writers",
                                 "readrs",
                                 "nors"
  })).init();

  measurementP := NEW(DiscreteParam,
                      tool := Tool.PostSpice,
                      nm := "measure",
                      flag := "measure",
                      vals := RT( TA { "bitlinebump",
                                       "hitlinedroop" }));
                      

  progP   := NEW(DiscreteParam,
                 tool := Tool.Script,
                 nm := "prog",
                 flag := "prog",
                 vals := RT( TcamPrograms.Names )
  ).init();

  cfgP := NEW(IntParam,
              tool := Tool.SpiceBuilder,
              nm   := "cfg",
              flag := "cfg",
              min  := 0,
              max  := 16_fff,
              base := 16);
  
PROCEDURE AllCornerSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, 
            cover := NARROW(cornerP,DiscreteParam).vals (*RT(TA { "tttt" })*)
    ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.50d0,  500.0d6 } ),
            max     := RL ( LR01 {  1.25d0, 2500.0d6 } ),
            minStep := RL ( LR01 { 0.010d0,    5.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 { 0.100d0,  200.0d6 })));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 125.0d0, step := 40.0d0));
  END AllCornerSim;

PROCEDURE SimulatorSim() =
  BEGIN
    (* what's going on with temperature? *)
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa", "hspice" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.70d0, 1000.0d6 } ),
            max     := RL ( LR01 {  1.05d0, 1500.0d6 } ),
            minStep := RL ( LR01 { 0.050d0,   10.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 { 0.100d0,   50.0d6 })));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 125.0d0, step := 40.0d0));
  END SimulatorSim;

  (* ********************  variation sims below here  ******************** *)

  (* get the correct dimension names for pessimization *)
TYPE
  DefDimGetter = Schmoozer.DimGetter OBJECT OVERRIDES
    dims := DDGdims;
  END;

PROCEDURE DDGdims(<*UNUSED*>ddg : DefDimGetter;
                  varName, modelName : TEXT) : REF ARRAY OF TEXT =
  BEGIN
    RETURN TcamSimulationSDG64.GetVarNames(varName, modelName)
  END DDGdims;

PROCEDURE SingleVar() =
  BEGIN
    (* variation sim! *)
    varSfx := "_var";
    
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Variety, param := progP  , cover := RT(TA {  "var" })));
    Add(NEW(Variety, param := measurementP  , cover := RT(TA {  "bitlinebump" })));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := -40.0d0, step := 40.0d0));
    Add(NEW(Sweep, param := vddP, 
            min := 0.75d0, max := 0.75d0, step := 0.1d0));
    Add(NEW(Sweep, param := clkP, 
            min := 1000.0d6, max := 1000.0d6, step := 100.0d6));
    Add(NEW(SpecificInt, param := cfgP,
            v := RI(IA { 16_000 })));
    Add(NEW(VarOpt,
            stopParam := vddP,
            stopRatio := 0.6d0,
            optMult   := -1.0d0, (* maximize *)
            varName := "stability",
            dims := NEW(DefDimGetter),
            radius := RL(
               LA { 2.0d0 }
    )));
  END SingleVar;

PROCEDURE MaybeFailVar() =
  BEGIN
    (* variation sim! *)
    varSfx := "_var";
    
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Variety, param := progP  , cover := RT(TA {  "var" })));
    Add(NEW(Variety, param := measurementP  , cover := RT(TA {  "bitlinebump" })));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := -40.0d0, step := 40.0d0));
    Add(NEW(Sweep, param := vddP, 
            min := 0.600d0, max := 0.600d0, step := 0.1d0));
    Add(NEW(Sweep, param := clkP, 
            min := 1000.0d6, max := 1000.0d6, step := 100.0d6));
    Add(NEW(SpecificInt, param := cfgP,
            v := RI(IA { 16_000 })));
    Add(NEW(VarOpt,
            stopParam := vddP,
            stopRatio := 0.6d0,
            optMult   := -1.0d0, (* maximize *)
            varName := "stability",
            dims := NEW(DefDimGetter),
            radius := RL(
               LA { 5.5d0 }
    )));
  END MaybeFailVar;

PROCEDURE MultiVar() =
  BEGIN
    (* variation sim! *)
    varSfx := "_var";
    
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Variety, param := progP  , cover := RT(TA {  "var" })));
    Add(NEW(Variety, param := measurementP  , cover := RT(TA {  "bitlinebump" })));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 100.0d0, step := 140.0d0));
    Add(NEW(Sweep, param := vddP, 
            min := 0.45d0, max := 0.65d0, step := 0.025d0));
    Add(NEW(Sweep, param := clkP, 
            min := 500.0d6, max := 500.0d6, step := 100.0d6));
    Add(NEW(SpecificInt, param := cfgP,
            v := RI(IA { 16_000, 16_fc0 })));
    Add(NEW(VarOpt,
            dims := NEW(DefDimGetter),
            optMult   := -1.0d0, (* maximize *)
            varName := "stability",
            radius := RL(
               LA { 0.0d0, 2.5d0, 5.5d0, 6.0d0 }

    )));
  END MultiVar;

PROCEDURE LeakVar() =
  BEGIN
    (* variation sim! *)
    varSfx := "_leakvar";
    
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io"       })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "rfff", "tttt"       }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa"       })));
    Add(NEW(Variety, param := progP  , cover := RT(TA {  "singlehit" })));
    Add(NEW(Variety, param := measurementP  , cover := RT(TA {  "hitlinedroop" })));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 110.0d0, step := 150.0d0));
    Add(NEW(Sweep, param := vddP, 
            min := 0.45d0, max := 1.05d0, step := 0.300d0));
    Add(NEW(Sweep, param := clkP, 
            min := 250.0d6, max := 250.0d6, step := 100.0d6));
    Add(NEW(SpecificInt, param := cfgP,
            v := RI(IA { 16_fc0, 16_fc7 })));
    Add(NEW(VarOpt,
            dims := NEW(DefDimGetter),
            optMult   := +1.0d0, (* minimize *)
            stopParam := vddP,
            stopRatio := 0.2d0,
            varName := "hit_leakage",
            radius := RL(
               LA { 0.0d0, 1.0d0, 3.0d0, 5.0d0, 5.5d0, 6.0d0 }
    )));
  END LeakVar;

PROCEDURE LeakVar2() =
  BEGIN
    (* variation sim! *)
    varSfx := "_leakvar";
    
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io"       })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt", "ffff", "rfff", "pfff"       }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa"       })));
    Add(NEW(Variety, param := progP  , cover := RT(TA {  "singlehit" })));
    Add(NEW(Variety, param := measurementP  , cover := RT(TA {  "hitlinedroop" })));
    Add(NEW(Sweep, param := tempP, 
            min := 110.0d0, max := 110.0d0, step := 150.0d0));
    Add(NEW(Sweep, param := vddP, 
            min := 0.450d0, max := 0.750d0, step := 0.100d0));
    Add(NEW(Sweep, param := clkP, 
            min := 250.0d6, max := 250.0d6, step := 100.0d6));
    Add(NEW(SpecificInt, param := cfgP,
            v := RI(IA { 16_fc0, 16_fc7 })));
    Add(NEW(VarOpt,
            dims := NEW(DefDimGetter),
            optMult   := +1.0d0, (* minimize *)
            stopParam := vddP,
            stopRatio := 0.2d0,
            varName := "hit_leakage",
            radius := RL(
               LA { 0.0d0, 4.5d0, 5.5d0, 6.0d0 }
    )));
  END LeakVar2;

PROCEDURE LeakVarBroken() =
  BEGIN
    (* variation sim! *)
    varSfx := "_leakvar";
    
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io"       })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA {   "ffff"       }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa"       })));
    Add(NEW(Variety, param := progP  , cover := RT(TA {  "singlehit" })));
    Add(NEW(Variety, param := measurementP  , cover := RT(TA {  "hitlinedroop" })));
    Add(NEW(Sweep, param := tempP, 
            min := 110.0d0, max := 110.0d0, step := 150.0d0));
    Add(NEW(Sweep, param := vddP, 
            min := 0.450d0, max := 0.450d0, step := 0.100d0));
    Add(NEW(Sweep, param := clkP, 
            min := 250.0d6, max := 250.0d6, step := 100.0d6));
    Add(NEW(SpecificInt, param := cfgP,
            v := RI(IA { 16_fc7 })));
    Add(NEW(VarOpt,
            dims := NEW(DefDimGetter),
            optMult   := +1.0d0, (* minimize *)
            stopParam := vddP,
            stopRatio := 0.2d0,
            varName := "hit_leakage",
            radius := RL(
               LA { 0.0d0, 4.0d0, 5.0d0, 6.0d0 }
    )));
  END LeakVarBroken;

  (**********************************************************************)
  
PROCEDURE XATempSim() =
  BEGIN
    (* what's going on with temperature? *)
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa", "hspice" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.65d0, 1000.0d6 } ),
            max     := RL ( LR01 {  1.00d0, 2200.0d6 } ),
            minStep := RL ( LR01 { 0.005d0,   15.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 { 0.050d0,  150.0d6 })));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 125.0d0, step := 32.5d0));
  END XATempSim;

PROCEDURE WideSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo,
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.00d0,  200.0d6 } ),
            max     := RL ( LR01 {  1.50d0, 3200.0d6 } ),
            minStep := RL ( LR01 { 0.0075d0,  10.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 { 0.100d0,  200.0d6 })));
    Add(NEW(Sweep, param := tempP,
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
  END WideSim;

PROCEDURE SpfSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.50d0,  800.0d6 } ),
            max     := RL ( LR01 {  1.00d0, 2200.0d6 } ),
            minStep := RL ( LR01 { 0.006d0,   20.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 { 0.048d0,  160.0d6 })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
    Add(NEW(SweepSpecific, param := spfP,
            v := RL(LA { 0.10d0, 0.03d0, 0.01d0, 0.003d0, 0.001d0, 0.0003d0, 0.0d0 })));
  END SpfSim;

PROCEDURE QuickSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.30d0,  600.0d6 } ),
            max     := RL ( LR01 {  1.00d0, 2600.0d6 } ),
            minStep := RL ( LR01 {  0.001d0,   4.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  0.050d0,  200.0d6 })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
  END QuickSim;

PROCEDURE LatchSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));

    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs", "readrs", "writers", "nors" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.50d0,  300.0d6 } ),
            max     := RL ( LR01 {  1.20d0, 5000.0d6 } ),
            minStep := RL ( LR01 {  0.020d0,  130.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  0.070d0,  470.0d6 })));
  END LatchSim;

PROCEDURE OutputHoldSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
    Add(NEW(Sweep, param := assertHoldP, 
            min := 0.0d0, max := 0.50d0, step := 0.25d0));

    (* for now: bigtime hack, add this last: *)
    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs", "nors" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.65d0, 1500.0d6 } ),
            max     := RL ( LR01 {  1.05d0, 5000.0d6 } ),
            minStep := RL ( LR01 {  0.015d0,  140.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  0.060d0,  525.0d6 })));
  END OutputHoldSim;

PROCEDURE OutputHoldSchmoo() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
    Add(NEW(Sweep,
            param := vddP, min := 0.85d0, max := 1.15d0, step := 0.10d0));

    (* for now: bigtime hack, add this last: *)
    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs", "nors", "readrs" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    assertHoldP,     clkP } ),
            min     := RL ( LR01 {  0.00d0, 1500.0d6 } ),
            max     := RL ( LR01 {  2.00d0, 6800.0d6 } ),
            minStep := RL ( LR01 {  0.014d0,   80.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  0.140d0,  800.0d6 })));
  END OutputHoldSchmoo;

PROCEDURE TempHVSchmoo() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep,
            param := vddP, min := 1.10d0, max := 1.10d0, step := 0.10d0));
    Add(NEW(Sweep, param := assertHoldP, 
            min := 0.0d0, max := 0.50d0, step := 0.50d0));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    tempP,     clkP } ),
            min     := RL ( LR01 {  -40.0d0, 4200.0d6 } ),
            max     := RL ( LR01 {  0.00d0, 5800.0d6 } ),
            minStep := RL ( LR01 {  0.4d0,   6.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  4.0d0,  60.0d6 })));
  END TempHVSchmoo;

PROCEDURE WideTempHVSchmoo() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep,
            param := vddP, min := 1.10d0, max := 1.10d0, step := 0.10d0));
    Add(NEW(Sweep, param := assertHoldP, 
            min := 0.0d0, max := 0.50d0, step := 0.50d0));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    tempP,     clkP } ),
            min     := RL ( LR01 {  -40.0d0,  3200.0d6 } ),
            max     := RL ( LR01 {  120.00d0, 5800.0d6 } ),
            minStep := RL ( LR01 {  1.6d0,   14.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  16.0d0,  140.0d6 })));
  END WideTempHVSchmoo;

PROCEDURE WideTempVSchmoo() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep,
            param := vddP, min := 0.55d0, max := 1.10d0, step := 0.10d0));
    Add(NEW(Sweep, param := assertHoldP, 
            min := 0.0d0, max := 0.50d0, step := 0.50d0));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    tempP,     clkP } ),
            min     := RL ( LR01 {  -40.0d0,  320.0d6 } ),
            max     := RL ( LR01 {  120.00d0, 5800.0d6 } ),
            minStep := RL ( LR01 {  1.0d0,   0.0d0 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.01d0 } ),
            maxStep := RL ( LR01 {  16.0d0,  200.0d6 })));
  END WideTempVSchmoo;

PROCEDURE WideOutputHoldSchmoo() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
    Add(NEW(Sweep,
            param := vddP, min := 0.85d0, max := 1.15d0, step := 0.10d0));

    (* for now: bigtime hack, add this last: *)
    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs", "nors", "readrs", "writers" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    assertHoldP,     clkP } ),
            min     := RL ( LR01 {  -1.00d0, 1500.0d6 } ),
            max     := RL ( LR01 {  3.00d0, 6800.0d6 } ),
            minStep := RL ( LR01 {  0.028d0,  160.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  0.140d0,  800.0d6 })));
  END WideOutputHoldSchmoo;

PROCEDURE WideOutputHoldTimeSchmoo() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt", "ssss" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
    Add(NEW(Sweep,
            param := vddP, min := 0.45d0, max := 1.15d0, step := 0.10d0));

    (* for now: bigtime hack, add this last: *)
    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs", "nors" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    assertHoldTimeP,     clkP } ),
            min     := RL ( LR01 { -1.000d-9,  250.0d6 } ),
            max     := RL ( LR01 {  1.000d-9, 6800.0d6 } ),
            minStep := RL ( LR01 {  0.007d-9,    40.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  0.125d-9,  800.0d6 })));
  END WideOutputHoldTimeSchmoo;

PROCEDURE OutHoldCorners() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt", "rsss", "rfff", "rfsf", "rssf", "rsfs" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 110.0d0, step := 85.0d0));
    Add(NEW(Sweep,
            param := assertHoldP, min := 0.00d0, max := 1.00d0, step := 0.25d0));

    (* for now: bigtime hack, add this last: *)
    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "nors" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.25d0,  250.0d6 } ),
            max     := RL ( LR01 {  1.25d0, 8000.0d6 } ),
            minStep := RL ( LR01 {  0.010d0,  10.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.01d0 } ),
            maxStep := RL ( LR01 {  0.100d0,  800.0d6 })));
  END OutHoldCorners;

PROCEDURE OutputHoldSchmooWriteRS() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
    Add(NEW(Sweep,
            param := vddP, min := 0.85d0, max := 1.15d0, step := 0.10d0));

    (* for now: bigtime hack, add this last: *)
    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "writers" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    assertHoldP,     clkP } ),
            min     := RL ( LR01 {  0.00d0, 1500.0d6 } ),
            max     := RL ( LR01 {  2.00d0, 6800.0d6 } ),
            minStep := RL ( LR01 {  0.014d0,   80.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  0.140d0,  800.0d6 })));
  END OutputHoldSchmooWriteRS;

PROCEDURE QuickOutputHoldSchmoo() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
    Add(NEW(Sweep,
            param := vddP, min := 0.85d0, max := 1.15d0, step := 0.10d0));

    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    assertHoldP,     clkP } ),
            min     := RL ( LR01 {  0.00d0, 1500.0d6 } ),
            max     := RL ( LR01 {  1.50d0, 6000.0d6 } ),
            minStep := RL ( LR01 {  0.015d0,    1.0d6 } ),
            minRatio:= RL ( LR01 {  0.000d0,  001.0d-2 } ),
            maxStep := RL ( LR01 {  0.090d0,  525.0d6 })));
  END QuickOutputHoldSchmoo;

PROCEDURE SuperQuickOutputHoldSchmoo() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
    Add(NEW(Sweep,
            param := vddP, min := 0.85d0, max := 0.85d0, step := 0.10d0));

    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    assertHoldP,     clkP } ),
            min     := RL ( LR01 {  0.00d0, 1500.0d6 } ),
            max     := RL ( LR01 {  1.50d0, 6000.0d6 } ),
            minStep := RL ( LR01 {  0.090d0,  525.0d6 } ),
            minRatio:= RL ( LR01 {  0.000d0,  000.0d-2 } ),
            maxStep := RL ( LR01 {  0.090d0,  525.0d6 })));
  END SuperQuickOutputHoldSchmoo;

PROCEDURE Quick3DSchmoo() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP02 {  assertHoldP,      clkP,    vddP } ),
            min     := RL ( LR02 {       0.00d0,   500.0d6, 0.450d0 } ),
            max     := RL ( LR02 {       1.00d0,  6000.0d6, 1.150d0 } ),
            minStep := RL ( LR02 {      0.010d0,   100.0d6, 0.025d0 } ),
            minRatio:= RL ( LR02 {      0.000d0,  000.0d-2, 0.000d0 } ),
            maxStep := RL ( LR02 {      0.090d0,   525.0d6, 0.200d0 })));
  END Quick3DSchmoo;

PROCEDURE BigSchmoo() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt", "rsss", "rfff", "rfsf", "rssf", "rsfs" }) ));
    Add(NEW(Sweep, param := assertHoldP, 
            min := 0.5d0, max := 0.5d0, step := 1.0d0));
    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP02 {        tempP,      clkP,    vddP } ),
            min     := RL ( LR02 {       -50.0d0,   500.0d6, 0.450d0 } ),
            max     := RL ( LR02 {       125.00d0,  7000.0d6, 1.150d0 } ),
            minStep := RL ( LR02 {      5.0d0,   100.0d6, 0.025d0 } ),
            minRatio:= RL ( LR02 {      0.000d0,  000.0d-2, 0.000d0 } ),
            maxStep := RL ( LR02 {      25.0d0,   525.0d6, 0.200d0 })));
  END BigSchmoo;

PROCEDURE HoldSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));

    Add(NEW(Sweep,
            param := vddP, min := 0.65d0, max := 1.15d0, step := 0.10d0));
    
    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs", "readrs", "writers", "nors" })));

    Add(NEW(Schmoo, 
            param   := RP ( RP01 {   holdP,     clkP } ),
            min     := RL ( LR01 {  0.00d0, 1000.0d6 } ),
            max     := RL ( LR01 {  1.00d0, 5000.0d6 } ),
            minStep := RL ( LR01 {  0.004d0,  12.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  0.06d0,  200.0d6 })));
  END HoldSim;

PROCEDURE ThreeCornerSim() =
  BEGIN
    (* what's going on with temperature? *)
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "ssss", "tttt", "ffff" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.50d0,  800.0d6 } ),
            max     := RL ( LR01 {  1.30d0, 2600.0d6 } ),
            minStep := RL ( LR01 { 0.010d0,   30.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 { 0.100d0,  300.0d6 })));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 125.0d0, step := 32.5d0));
  END ThreeCornerSim;

PROCEDURE SevenCornerSim() =
  BEGIN
    (* what's going on with temperature? *)
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := 
          RT(TA { "rfff","tttt","rffs","rfsf","rsss","rssf","rsfs"  }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.50d0,  800.0d6 } ),
            max     := RL ( LR01 {  1.30d0, 2600.0d6 } ),
            minStep := RL ( LR01 { 0.020d0,   45.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 { 0.100d0,  225.0d6 })));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 125.0d0, step := 65.0d0));
  END SevenCornerSim;

PROCEDURE TestSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.75d0, 1000.0d6 } ),
            max     := RL ( LR01 {  0.85d0, 1010.0d6 } ),
            minStep := RL ( LR01 { 0.001d0,    1.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 { 0.100d0,   10.0d6 })));
    Add(NEW(Sweep, param := tempP, 
            min := 0.0d0, max := 0.0d0, step := 20.0d0));
  END TestSim;

PROCEDURE CfgSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP ,     clkP } ),
            min     := RL ( LR01 {  0.45d0 ,  300.0d6 } ),
            max     := RL ( LR01 {  1.00d0 , 3000.0d6 } ),
            minStep := RL ( LR01 {  0.010d0,   50.0d6 } ),
            minRatio:= RL ( LR01 {  0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 {  0.100d0,  200.0d6 })));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 110.0d0, step := 75.0d0));
    Add(NEW(SpecificInt, param := cfgP,
            v := RI(IA { 16_cf9, 16_fc0, 16_000 })))
  END CfgSim;

PROCEDURE SingleSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP ( RP01 {    vddP,     clkP } ),
            min     := RL ( LR01 {  0.85d0, 1000.0d6 } ),
            max     := RL ( LR01 {  0.85d0, 1000.0d6 } ),
            minStep := RL ( LR01 { 0.100d0,   10.0d6 } ),
            minRatio:= RL ( LR01 { 0.000d0,  000.0d0 } ),
            maxStep := RL ( LR01 { 0.100d0,   10.0d6 })));
    Add(NEW(Sweep, param := tempP, 
            min := 0.0d0, max := 0.0d0, step := 20.0d0));
  END SingleSim;

VAR
  varSfx := "";
  pp := NEW(ParseParams.T).init(Stdio.stderr);
BEGIN
  Debug.SetOptions(SET OF Debug.Options { Debug.Options.PrintThreadID });
  Schmoozer.Setup(pp,
      NEW(DiscreteParam,
          nm := "simulator",
          flag := "f",
          vals := RT(Sim.Names)).init()
      );
  
  Schmoozer.Process(pp,
                    ARRAY OF Schmooze.T {
    Schmooze.T { HoldSim                   , "hold" },
    Schmooze.T { OutputHoldSim             , "outhold" },
    Schmooze.T { OutputHoldSchmoo          , "outholdschmoo" },
    Schmooze.T { OutputHoldSchmooWriteRS   , "outholdschmoowriters" },
    Schmooze.T { WideOutputHoldSchmoo      , "wideoutholdschmoo" },
    Schmooze.T { WideOutputHoldTimeSchmoo  , "wideoutholdtimeschmoo" },
    Schmooze.T { QuickOutputHoldSchmoo     , "quickoutholdschmoo" },
    Schmooze.T { SuperQuickOutputHoldSchmoo, "superquickoutholdschmoo" },
    Schmooze.T { Quick3DSchmoo             , "quick3Dschmoo" },
    Schmooze.T { SevenCornerSim            , "7corners" },
    Schmooze.T { ThreeCornerSim            , "3corners" },
    Schmooze.T { AllCornerSim              , "allcorners" },
    Schmooze.T { OutHoldCorners            , "outholdcorners" },
    
    Schmooze.T { WideSim                   , "wide" },
    Schmooze.T { XATempSim                 , "xatemp" },
    Schmooze.T { QuickSim                  , "quick" },
    Schmooze.T { LatchSim                  , "latch" },
    Schmooze.T { SingleSim                 , "single" },
    Schmooze.T { SpfSim                    , "spf" },
    Schmooze.T { TestSim                   , "test" },
    Schmooze.T { BigSchmoo                 , "big" },
    Schmooze.T { TempHVSchmoo              , "temphv" },
    Schmooze.T { WideTempHVSchmoo          , "widetemphv" },
    Schmooze.T { WideTempVSchmoo           , "widetempv" },
    Schmooze.T { CfgSim                    , "cfg" },
    Schmooze.T { SimulatorSim              , "simulator" },

    (* variation simulations follow *)
    Schmooze.T { SingleVar                 , "singlevar" },
    Schmooze.T { MultiVar                  , "multivar" },
    Schmooze.T { MaybeFailVar              , "maybefailvar" },
    Schmooze.T { LeakVar                   , "leakvar" },
    Schmooze.T { LeakVarBroken             , "leakvarbroken" },
    Schmooze.T { LeakVar2                  , "leakvar2" }
  
  });

  IF pp.keywordPresent("-design") THEN
    WITH design = pp.getNext() DO
      IF TE(design, "sdg64") THEN
        Schmoozer.SetExtraCmdHack(F(" -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2%s -design sdg64",varSfx))
      ELSIF TE(design, "andrew") THEN
          (* skip *)
      ELSE
        Debug.Error(F("Unknown design \"%s\"", design))
      END
    END
  END;
  
  Schmoozer.RunAll(pp)
END SchmoozerSim.

  


  
