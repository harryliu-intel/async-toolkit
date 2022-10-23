INTERFACE TechConfig;

TYPE
  Tech = { N5, P1276p4, N3, N3E, P1278p3 };
  (* supported technologies *)

  Corp = { Tsmc, Intc };
  
  Tran = { Elvt,
           Ulvt,
           Ulvtll,
           Lvt,
           Lvtll,
           Svt,
           Svtll (* use for Hvt in P1278p3 *)
  };
  (* all known transistor thresholds (superset of all techs) *)
  
  Mode = { Dyn, Leak };
  (* run mode: dynamic power or leakage power *)
  
  Phaz = { Setup, Simulate, Convert, Clean, Measure };
  (* run phase:

     Setup    - make input file
     Simulate - run circuit simulator
     Convert  - convert data output to aspice format
     Clean    - remove unneeded files
     Measure  - make performance measurements required on aspice data

     Use -all on command line to get all phases
  *)
  
  Simu = { Xa, Hspice };
  (* circuit simulator: xa fully supported, hspice coming later *)
  
  Corn = { TT, SS, FF, SF, FS };
  (* short names for the five supported simulation corners *)

  Gate = { Xor, Buf, Aoi, Oai };
  
CONST
  (* the following are string names, to be used from command line, etc. *)
  TranNames = ARRAY Tran OF TEXT
              { "elvt", "ulvt", "ulvtll", "lvt", "lvtll", "svt", "svtll" };

  ModeNames = ARRAY Mode OF TEXT { "dyn" ,  "leak" };

  PhazNames = ARRAY Phaz OF TEXT
              { "setup", "simulate", "convert", "clean", "measure" };

  SimuNames = ARRAY Simu OF TEXT { "xa",    "hspice" };

  CornNames = ARRAY Corn OF TEXT { "tt", "ss", "ff", "sf", "fs" };

  GateNames = ARRAY Gate OF TEXT { "xor", "buf", "aoi", "oai" };
  (* should not ask for an oai, should only ask for aoi *)

  TechNames = ARRAY Tech OF TEXT { "n5", "1276p4", "n3", "n3e", "1278p3" };

  TechCorp  = ARRAY Tech OF Corp { Corp.Tsmc,
                                   Corp.Intc,
                                   Corp.Tsmc,
                                   Corp.Tsmc,
                                   Corp.Intc };

  Gate1 = ARRAY Gate OF Gate
            { Gate.Xor, Gate.Buf, Gate.Oai, Gate.Aoi };
  (* second gate type for each first gate --
     note that oai is really not supported as a first gate *)

  SupportedFanouts = SET OF [ 0 .. 8 ] { 1, 2, 3, 4, 8 };
  
CONST
  CorpNames = ARRAY Corp OF TEXT { "TSMC", "INTC" };
  
END TechConfig.
