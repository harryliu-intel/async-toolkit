INTERFACE TechConfig;
IMPORT Pathname;

TYPE
  Tech = { N5,
           P1276p4,
           P1276p4_g1m,
           P1276p4_aml1,
           P1276p4_aml2,
           N3,
           N3E,
           P1278p3,
           P1278p3_i0m };
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

  Gate = { Xor,    (* regular XOR2 *)
           XorAlt, (* alternative XOR2 *)
           Buf,    (* buffer *)
           Aoi,    (* AOI gate *)
           Oai,     (* don't use this -- alternates with Aoi *)

           Xor_Z1_0p0sigma,  (* special variation sim w/o variation *)
           Xor_Z1_5p3sigma,  (* special variation sim w/ 5.3 sigma variation *)
           Xor_Z2_0p0sigma,  (* special variation sim w/o variation *)
           Xor_Z2_5p3sigma,  (* special variation sim w/ 5.3 sigma variation *)

           Aoi_Z1_0p0sigma,  (* special variation sim w/o variation *)
           Aoi_Z1_5p3sigma,  (* special variation sim w/ 5.3 sigma variation *)
           Aoi_Z2_0p0sigma,  (* special variation sim w/o variation *)
           Aoi_Z2_5p3sigma,  (* special variation sim w/ 5.3 sigma variation *)

           Oai_Z1_0p0sigma,  (* special variation sim w/o variation *)
           Oai_Z1_5p3sigma,  (* special variation sim w/ 5.3 sigma variation *)
           Oai_Z2_0p0sigma,  (* special variation sim w/o variation *)
           Oai_Z2_5p3sigma,  (* special variation sim w/ 5.3 sigma variation *)

           Xor_Z1,           (* Z1 XOR, no variation *)
           Xor_Z2,           (* Z2 XOR, no variation *)
           Xor_Z3            (* Z3 XOR, no variation *)
  
  };
  
CONST
  (* the following are string names, to be used from command line, etc. *)
  TranNames = ARRAY Tran OF TEXT
              { "elvt", "ulvt", "ulvtll", "lvt", "lvtll", "svt", "svtll" };

  ModeNames = ARRAY Mode OF TEXT { "dyn" ,  "leak" };

  PhazNames = ARRAY Phaz OF TEXT
              { "setup", "simulate", "convert", "clean", "measure" };

  SimuNames = ARRAY Simu OF TEXT { "xa",    "hspice" };

  CornNames = ARRAY Corn OF TEXT { "tt", "ss", "ff", "sf", "fs" };

  GateNames = ARRAY Gate OF TEXT { "xor", "xoralt", "buf", "aoi", "oai",
                                   "xor_z1_0p0sigma", "xor_z1_5p3sigma",
                                   "xor_z2_0p0sigma", "xor_z2_5p3sigma",
                                   "aoi_z1_0p0sigma", "aoi_z1_5p3sigma",
                                   "aoi_z2_0p0sigma", "aoi_z2_5p3sigma",
                                   "oai_z1_0p0sigma", "oai_z1_5p3sigma",
                                   "oai_z2_0p0sigma", "oai_z2_5p3sigma",

                                   "xor_z1", "xor_z2", "xor_z3"
                                   };
  (* should not ask for an oai, should only ask for aoi *)

  TechNames = ARRAY Tech OF TEXT { "n5",
                                   "1276p4",
                                   "1276p4_g1m",
                                   "1276p4_aml1",
                                   "1276p4_aml2",
                                   "n3",
                                   "n3e",
                                   "1278p3",
                                   "1278p3_i0m" };

  TemplateNames = ARRAY Gate OF Pathname.T {

  (* basic tech comparisons *)
  "ckt.sp",
  "ckt.sp",
  "ckt.sp",
  "ckt.sp",
  "ckt.sp",
  
  (* tech comparisons under variation with XOR *)
  "ckt_varxor.sp",
  "ckt_varxor.sp",
  "ckt_varxor.sp",
  "ckt_varxor.sp",

  (* tech comparisons under variation with AOI/OAI *)
  "ckt_varaoi.sp",
  "ckt_varaoi.sp",
  "ckt_varaoi.sp",
  "ckt_varaoi.sp",
  "ckt_varaoi.sp",
  "ckt_varaoi.sp",
  "ckt_varaoi.sp",
  "ckt_varaoi.sp",

  "ckt_varosc.sp",
  "ckt_varosc.sp",
  "ckt_varosc.sp"
  };

  TechCorp  = ARRAY Tech OF Corp { Corp.Tsmc,
                                   Corp.Intc,
                                   Corp.Intc,
                                   Corp.Intc,
                                   Corp.Intc,
                                   Corp.Tsmc,
                                   Corp.Tsmc,
                                   Corp.Intc,
                                   Corp.Intc };

  Gate1 = ARRAY Gate OF Gate
  { Gate.Xor, Gate.XorAlt, Gate.Buf, Gate.Oai, Gate.Aoi,

    Gate.Xor_Z1_0p0sigma, Gate.Xor_Z1_5p3sigma,
    Gate.Xor_Z2_0p0sigma, Gate.Xor_Z2_5p3sigma,

    Gate.Oai_Z1_0p0sigma, Gate.Oai_Z1_5p3sigma,
    Gate.Oai_Z2_0p0sigma, Gate.Oai_Z2_5p3sigma,

    Gate.Aoi_Z1_0p0sigma, Gate.Aoi_Z1_5p3sigma,
    Gate.Aoi_Z2_0p0sigma, Gate.Aoi_Z2_5p3sigma,

    Gate.Xor_Z1, Gate.Xor_Z2, Gate.Xor_Z3

    };
  (* second gate type for each first gate --
     note that oai is really not supported as a first gate *)

  SupportedFanouts = SET OF [ 0 .. 8 ] { 1, 2, 3, 4, 8 };
  
CONST
  CorpNames = ARRAY Corp OF TEXT { "TSMC", "INTC" };

CONST DefSimRoot = "circuit";

TYPE
  T = RECORD
    tech   : Tech;
    tran   : Tran;
    mode   : Mode;
    simu   : Simu;
    corn   : Corn;
    gate   : Gate;
    fanout : CARDINAL := 1;

    volt     := 0.0d0;
    temp     := 0.0d0;
    sigma    := 0.0d0; (* not always used *)
    stdcells := "std";
    
    nanoseconds     : LONGREAL; (* length of sim in ns *)
    timestep        : LONGREAL; (* in seconds *)
    
    workDir         : Pathname.T;
    createWorkDir   : BOOLEAN;
    templateDir     : Pathname.T;
    
    phazz := SET OF Phaz { Phaz.Setup };
    
    hspiceModelRoot : Pathname.T;
    hspiceModel     : Pathname.T;
    hspiceModelName := "default";

    hspiceLibModels : Pathname.T :=
        "/p/hdk/cad/pdk/pdk783_r0.9_23ww26.5_alpha/cmi/hspice/cmi/lnx86/64bit";
    (* what is this file? *)

    pdmiLib         : Pathname.T;
    simRoot := DefSimRoot;
    xaPath     : Pathname.T := "/p/hdk/cad/xa/U-2023.03-2/bin/";
    hspicePath : Pathname.T := "/p/hdk/cad/hspice/U-2023.03-2/hspice/bin/";

    para : BOOLEAN; (* parasitic simulation yes/no *)
  END;

END TechConfig.
