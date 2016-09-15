MODULE SchmoozerSim EXPORTS Main;
IMPORT Schmoozer, Schmooze;

IMPORT ProbeMode, Sim;

FROM Schmoozer IMPORT DiscreteParam, RealParam;
FROM Schmoozer IMPORT Variety, Sweep, Schmoo, SweepSpecific;
FROM Schmoozer IMPORT LA, TA, RP01, LR01, RP02, LR02;
FROM Schmoozer IMPORT Tool;
FROM Schmoozer IMPORT Add, RL, RT, RP;

FROM DefParams IMPORT vddP, clkP, tempP, holdP;
FROM Schmoozer IMPORT simP;

VAR
  probeP  := NEW(DiscreteParam,
                 nm := "probemode",
                 flag := "probemode",
                 vals := RT(ProbeMode.Names)).init();

  cornerP := NEW(DiscreteParam,
                 nm := "corner",
                 flag := "m",
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
            min     := RL ( LR01 {  0.50d0,  600.0d6 } ),
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

BEGIN

  Schmoozer.Setup(
      NEW(DiscreteParam,
          nm := "simulator",
          flag := "f",
          vals := RT(Sim.Names)).init()
      );
  
  Schmoozer.Process( ARRAY OF Schmooze.T {
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
    Schmooze.T { SimulatorSim              , "simulator" }
  });

  Schmoozer.RunAll()
END SchmoozerSim.

  


  
