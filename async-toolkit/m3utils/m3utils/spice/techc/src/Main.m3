MODULE Main;

(*
  Technology comparison across

  P1276P4
  N5
  N3 (vaporware process)
  N3E

  including all provided transistor thresholds

  Author : mika.nystroem@intel.com
  October, 2022

*)

IMPORT ParseParams;
IMPORT Text;
IMPORT Debug;
IMPORT Stdio;
FROM Fmt IMPORT F, Int, FN; IMPORT Fmt;
IMPORT OSError;
IMPORT AL;
IMPORT Process;
IMPORT Pathname;
IMPORT TextTextTbl;
IMPORT TextSeq;
IMPORT Rd, Wr;
IMPORT FileRd, FileWr;
IMPORT CitTextUtils;
IMPORT Thread;
IMPORT ProcUtils;
IMPORT TextWr;
IMPORT Trace;
IMPORT LongRealSeq;
IMPORT Math;
IMPORT FS;
IMPORT Watchdog;
IMPORT Scan;
IMPORT Lex;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
      LR = Fmt.LongReal;

(* to add a new Tech to the system:

   Add to Tech, then update
   TechNames
   TranSufxs
   TechTranSufxs
   TechTranSizes
   TechHspiceModels
   TechHspiceModelRoots
   TechCornNames
   MapTech
   TechParaCellName
   TechPlugText
   TechStdCellPaths

   *** And if needed: ***
   Tran (if you have a new transistor type) 
     TranNames
     TranSufxs for existing processes with NIL in the new slot
     ApproxThresh
*)

CONST
  Verbose = FALSE ;
  
TYPE
  Tech = { N5, P1276p4, N3, N3E, P1278p3 };
  (* supported technologies *)
  
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
  
  Topo = { Intc, Tsmc }; (* circuit topo *)
  (* use the Intel or TSMC circuit topology for the XOR gate *)
  
  Corn = { TT, SS, FF, SF, FS };
  (* short names for the five supported simulation corners *)
  
CONST
  (* the following are string names, to be used from command line, etc. *)
  TechNames = ARRAY Tech OF TEXT { "n5", "1276p4", "n3", "n3e", "1278p3" };
  TranNames = ARRAY Tran OF TEXT { "elvt", "ulvt", "ulvtll", "lvt", "lvtll", "svt", "svtll" };
  ModeNames = ARRAY Mode OF TEXT { "dyn" ,  "leak" };
  PhazNames = ARRAY Phaz OF TEXT { "setup", "simulate", "convert", "clean", "measure" };
  SimuNames = ARRAY Simu OF TEXT { "xa",    "hspice" };
  TopoNames = ARRAY Topo OF TEXT { "intc",  "tsmc" };
  CornNames = ARRAY Corn OF TEXT { "tt", "ss", "ff", "sf", "fs" };

CONST DefProcDeadline = 15.0d0 * 60.0d0;
      (* give it 15 minutes for each subprocess step (circuit sim and aspice
         data conversion) *)
      ParasiticDeadlineMultiplier = 6.0d0;
        
VAR ProcDeadline := DefProcDeadline;
  
TYPE
  TranSufxs     = ARRAY Tran OF TEXT;

CONST
  N5TranSufxs =
    TranSufxs { "ch_elvt_mac",
                "ch_ulvt_mac",
                "ch_ulvtll_mac",
                "ch_lvt_mac",
                "ch_lvtll_mac",
                "ch_svt_mac",
                "ch_svtll_mac"
  };

  P1276p4TranSufxs =
    TranSufxs { NIL,      (* elvt *)
                "hpulvt", (* ulvt *)
                NIL,      (* ulvtll *)
                "hplvt",  (* lvt *)
                NIL,      (* lvtll *)
                "hpsvt",  (* svt *)
                NIL       (* svtll *)
  };

  N3TranSufxs = N5TranSufxs;

  N3ETranSufxs =
    TranSufxs { "ch_elvt_mac",
                "ch_ulvt_mac",
                "ch_ulvtll_mac",
                "ch_lvt_mac",
                "ch_lvtll_mac",
                "ch_svt_mac",
                NIL
  };

  P1278p3TranSufxs =
    TranSufxs { NIL,       (* elvt *)
                "hpbulvt", (* ulvt *)
                NIL,       (* ulvtll *)
                "hpblvt",  (* lvt *)
                NIL,       (* lvtll *)
                "hpbsvt",  (* svt *)
                "hpbhvt"   (* hvt = svtll *)
  };

  P1276p4TranSize = "L=0.014u W=0.06u";
  
  N5TranSize = "l=6n nfin=2 ppitch=0 fbound=9";

  N3TranSize = "l=3n nfin=2 ppitch=0";

  N3ETranSize = "l=3n nfin=2 ppitch=0 fbound=262";

  P1278p3TranSize = "w=2 l=14e-9 m=1 nf=1";
                
  TechTranSufxs = ARRAY Tech OF TranSufxs { N5TranSufxs,
                                            P1276p4TranSufxs,
                                            N3TranSufxs,
                                            N3ETranSufxs ,
                                            P1278p3TranSufxs };

  TechTranSizes = ARRAY Tech OF TEXT { N5TranSize,
                                       P1276p4TranSize,
                                       N3TranSize,
                                       N3ETranSize,
                                       P1278p3TranSize };

  N5HspiceModel = "cln5_1d2_sp_v1d1_2p2_usage.l";
  P1276p4HspiceModel = "p1276_4.hsp";
  N3HspiceModel = "cln3_1d2_sp_v1d0_2p2_usage.l";
  N3EHspiceModel = "cln3e_1d2_sp_v0d5_2p2_usage.l";
  P1278p3HspiceModel = "p1278_3.hsp";

  (************************************************************)  

  N5HspiceModelRoot = "/p/tech/n5/tech-release/v1.1.3/models/1P15M_1X_h_1Xb_v_1Xe_h_1Ya_v_1Yb_h_5Y_vhvhv_2Yy2R/hspice";

  P1276p4HspiceModelRoot = "/p/hdk/cad/pdk/pdk764_r0.5_22ww20.5/models/core/hspice/m17_6x_2ya_2yb_2yc_2yd_1ye_1ga_mim3x_1gb__bumpp";
  
  N3HspiceModelRoot = "/p/tech1/n3/tech-release/v1.0.10/models/1P18M_1X_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_5Y_hvhvh_2Yy2Yx1R1U_thin_curdl/hspice";

  N3EHspiceModelRoot = "/p/tech1/n3e/tech-release/v0.5.0/models/1P17M_1Xa_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_6Y_hvhvhv_2Yy2R_shdmim_ut-alrdl/hspice";
  
  P1278p3HspiceModelRoot = "/p/hdk/cad/pdk/pdk783_r0.3.1_22ww38.7/models/core/hspice/m16_2x_1xa_1xb_6ya_2yb_2yc_2yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp";
  
  (************************************************************)  
  
  TechHspiceModels = ARRAY Tech OF TEXT { N5HspiceModel,
                                          P1276p4HspiceModel,
                                          N3HspiceModel,
                                          N3EHspiceModel,
                                          P1278p3HspiceModel };

  TechHspiceModelRoots = ARRAY Tech OF TEXT { N5HspiceModelRoot,
                                              P1276p4HspiceModelRoot,
                                              N3HspiceModelRoot,
                                              N3EHspiceModelRoot,
                                              P1278p3HspiceModelRoot };

  N5CornNames = ARRAY Corn OF TEXT {
  "TTGlobalCorner_LocalMC_MOS_MOSCAP",
  "SSGlobalCorner_LocalMC_MOS_MOSCAP",
  "FFGlobalCorner_LocalMC_MOS_MOSCAP",
  "SFGlobalCorner_LocalMC_MOS_MOSCAP",
  "FSGlobalCorner_LocalMC_MOS_MOSCAP"
  };

  P1276p4CornNames = ARRAY Corn OF TEXT {
  "tttt",
  "psss",
  "pfff",
  "rssf",
  "rsfs"
  };

  P1278p3CornNames = ARRAY Corn OF TEXT {
  "tttt",
  "ss",
  "ff",
  "sf",
  "fs"
  };

  N3CornNames = N5CornNames;

  N3ECornNames = N5CornNames;

  TechCornNames = ARRAY Tech OF ARRAY Corn OF TEXT { N5CornNames,
                                                     P1276p4CornNames,
                                                     N3CornNames,
                                                     N3ECornNames,
                                                     P1278p3CornNames };



  (************************************************************)

  (* the below is for simulation with parasitics *)
  
  P1276p4StdCellRoot = "/p/hdk/cad/stdcells/g1m/22ww37.5_p1276d4_g1m_b.0.p3.core/spf/p1276d4_tttt_v0550_t100_pdn_max/";

  P1276p4StdCellPaths = ARRAY Tran OF TEXT {
    NIL,
    P1276p4StdCellRoot & "an/g1mbfn000aa1n02x5.spf",
    NIL,
    P1276p4StdCellRoot & "bn/g1mbfn000ab1n02x5.spf",
    NIL,
    P1276p4StdCellRoot & "cn/g1mbfn000ac1n02x5.spf",
    NIL
  };

  P1276p4StdCellNames = ARRAY Tran OF TEXT {
    NIL,
    "g1mbfn000aa1n02x5",
    NIL,
    "g1mbfn000ab1n02x5",
    NIL,
    "g1mbfn000ac1n02x5",
    NIL
  };

  P1278p3StdCellPaths = ARRAY Tran OF TEXT {
  NIL,
  "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk030_r2v0p0_uv2_pre/base_ulvt/spf/lib783_i0s_160h_50pp_base_ulvt_tttt_100c_cmax/i0sbfn000aa1n02x5.spf",
  NIL,
  "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk030_r2v0p0_uv2_pre/base_lvt/spf/lib783_i0s_160h_50pp_base_lvt_tttt_100c_cmax/i0sbfn000ab1n02x5.spf",
  NIL,
  "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk030_r2v0p0_uv2_pre/base_svt/spf/lib783_i0s_160h_50pp_base_svt_tttt_100c_cmax/i0sbfn000ac1n02x5.spf",
  "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk030_r2v0p0_uv2_pre/base_hvt/spf/lib783_i0s_160h_50pp_base_hvt_tttt_100c_cmax/i0sbfn000ad1n02x5.spf"
  };
  
  
  P1278p3StdCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0sbfn000aa1n02x5",
    NIL,
    "i0sbfn000ab1n02x5",
    NIL,
    "i0sbfn000ac1n02x5",
    "i0sbfn000ad1n02x5"
  };

  P1276p4PlugText = "";
  P1278p3PlugText = "";

  N5StdCellPaths = ARRAY Tran OF TEXT {
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_elvt_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_elvt_090a/tcbn05_bwph210l6p51cnod_base_elvt_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_ulvt_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_ulvt_090a/tcbn05_bwph210l6p51cnod_base_ulvt_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_ulvtll_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_ulvtll_090a/tcbn05_bwph210l6p51cnod_base_ulvtll_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_lvt_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_lvt_090a/tcbn05_bwph210l6p51cnod_base_lvt_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_lvtll_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_lvtll_090a/tcbn05_bwph210l6p51cnod_base_lvtll_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_svt_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_svt_090a/tcbn05_bwph210l6p51cnod_base_svt_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_svtll_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_svtll_090a/tcbn05_bwph210l6p51cnod_base_svtll_090a_lpe_typical_125c.spi" };

  N5StdCellNames = ARRAY Tran OF TEXT {
  "BUFFD1BWP210H6P51CNODELVT",
  "BUFFD1BWP210H6P51CNODULVT",
  "BUFFD1BWP210H6P51CNODULVTLL",
  "BUFFD1BWP210H6P51CNODLVT",
  "BUFFD1BWP210H6P51CNODLVTLL",
  "BUFFD1BWP210H6P51CNODSVT",
  "BUFFD1BWP210H6P51CNODSVTLL"
  };

  N5PlugText = "vcc vssx";

  N3StdCellPaths = ARRAY Tran OF TEXT { NIL, .. };
  N3StdCellNames = ARRAY Tran OF TEXT { NIL, .. };
  N3PlugText = "vcc vssx";

  N3EStdCellPaths = ARRAY Tran OF TEXT {
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_elvt_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_elvt_090b/tcbn03e_bwph169l3p48cpd_base_elvt_090b_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_ulvt_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_ulvt_090b/tcbn03e_bwph169l3p48cpd_base_ulvt_090b_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_ulvtll_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_ulvtll_090b/tcbn03e_bwph169l3p48cpd_base_ulvtll_090b_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_lvt_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_lvt_090b/tcbn03e_bwph169l3p48cpd_base_lvt_090b_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_lvtll_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_lvtll_090b/tcbn03e_bwph169l3p48cpd_base_lvtll_090b_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_svt_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_svt_090b/tcbn03e_bwph169l3p48cpd_base_svt_090b_lpe_typical_125c.spi",
  NIL
  };

  N3EStdCellNames = ARRAY Tran OF TEXT {
  "BUFFD1BWP169H3P48CPDELVT",
  "BUFFD1BWP169H3P48CPDULVT",
  "BUFFD1BWP169H3P48CPDULVTLL",
  "BUFFD1BWP169H3P48CPDLVT",
  "BUFFD1BWP169H3P48CPDLVTLL",
  "BUFFD1BWP169H3P48CPDSVT",
  NIL
  };
  
  N3EPlugText = "vcc vssx";

  (************************************************************)

  TechStdCellPaths = ARRAY Tech OF ARRAY Tran OF TEXT {
  N5StdCellPaths,
  P1276p4StdCellPaths,
  N3StdCellPaths,
  N3EStdCellPaths,
  P1278p3StdCellPaths
  };
  
  TechParaCellName = ARRAY Tech OF ARRAY Tran OF TEXT {
  N5StdCellNames,
  P1276p4StdCellNames,
  N3StdCellNames,
  N3EStdCellNames,
  P1278p3StdCellNames
  };

  TechPlugText = ARRAY Tech OF TEXT {
  N5PlugText,
  P1276p4PlugText,
  N3PlugText,
  N3EPlugText,
  P1278p3PlugText
  };
  
  StdPlugText = "vcc vssx";
    
  (************************************************************)

  ApproxThresh = ARRAY Tran OF LONGREAL { 0.100d0,
                                          0.250d0,
                                          0.300d0,
                                          0.350d0,
                                          0.400d0,
                                          0.450d0,
                                          0.500d0 };

  AbsZero = -273.15d0; (* absolute zero in degrees Celsius *)
  
CONST
  XaOptions =
    ".OPTION CMIFLAG=1 CMIUSRFLAG=3 PDMI=1\n"  &
    ".OPTION POST=fsdb PROBE=1\n" &
    ".OPTION XA_CMD=\"set_sim_level -level 6\"\n" &
    ".OPTION XA_CMD=\"set_wildcard_rule -match* one\"\n" &
    ".OPTION XA_CMD=\"set_message_option -limit 100\"\n" &
    ".OPTION XA_CMD=\"enable_print_statement 1\"\n" &
    ".OPTION XA_CMD=\"set_sim_case -case sensitive\"";

  HspiceOptions = "";

  SimOptions = ARRAY Simu OF TEXT { XaOptions, HspiceOptions };

TYPE
  RunPhase = PROCEDURE(READONLY c : Config);

CONST
  Phases = ARRAY Phaz OF RunPhase { DoSetup, DoSimulate, DoConvert, DoClean, DoMeasure };
  
PROCEDURE Lookup(str : TEXT; READONLY a : ARRAY OF TEXT) : CARDINAL =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF TE(str, a[i]) THEN RETURN i END
    END;
    VAR
      str := F("could not find %s among alternatives :");
    BEGIN
      FOR i := FIRST(a) TO LAST(a) DO
        str := str & F( " \"%s\"" , a[i])
      END;
      Debug.Error(str)
    END;
    <*ASSERT FALSE*>
  END Lookup;

<*NOWARN*>PROCEDURE MapTechN5(READONLY c : Config; map : TextTextTbl.T) =
  BEGIN
  END MapTechN5;

<*NOWARN*>PROCEDURE MapTech1276p4(READONLY c : Config; map : TextTextTbl.T) =
  BEGIN
  END MapTech1276p4;

<*NOWARN*>PROCEDURE MapTechN3(READONLY c : Config; map : TextTextTbl.T) =
  BEGIN
  END MapTechN3;

<*NOWARN*>PROCEDURE MapTechN3E(READONLY c : Config; map : TextTextTbl.T) =
  BEGIN
  END MapTechN3E;

<*NOWARN*>PROCEDURE MapTech1278p3(READONLY c : Config; map : TextTextTbl.T) =
  BEGIN
  END MapTech1278p3;

TYPE Mapper = PROCEDURE(READONLY c : Config; map : TextTextTbl.T);
     
CONST MapTech = ARRAY Tech OF Mapper { MapTechN5, MapTech1276p4, MapTechN3, MapTechN3E, MapTech1278p3 };

CONST CornDelay = ARRAY Corn OF LONGREAL { 1.0d0, 3.0d0, 0.8d0, 2.0d0, 2.0d0 };
      
PROCEDURE MapCommon(READONLY c : Config;  map : TextTextTbl.T)=
  VAR
    iter : TextTextTbl.Iterator;
    k, v : TEXT;
  BEGIN
    EVAL map.put("@HSPICE_MODEL_ROOT@", c.hspiceModelRoot);
    EVAL map.put("@HSPICE_MODEL@", c.hspiceModel);
    EVAL map.put("@TEMP@", LR(c.temp));
    EVAL map.put("@VOLT@", LR(c.volt));

    (* parasitic or not *)
    IF c.para THEN
      WITH cellname = TechParaCellName[c.tech][c.tran] DO
        <*ASSERT cellname # NIL*>
        EVAL map.put("@CELLNAME@", cellname)
      END;
      EVAL map.put("@PLUGTEXT@", TechPlugText[c.tech]);
      EVAL map.put("@INCLUDELIB@",
                   F(".include \"%s\"\n",
                     TechStdCellPaths[c.tech][c.tran]));
      EVAL map.put("@XORVCC@", ""); (* no vcc input for buffer *)
    ELSE
      (* not parasitic *)
      EVAL map.put("@CELLNAME@", "xor_cell_" & TopoNames[c.topo]);
      EVAL map.put("@PLUGTEXT@", StdPlugText);
      EVAL map.put("@INCLUDELIB@", "");
      EVAL map.put("@XORVCC@", "vcc"); (*  vcc input for XOR *)
    END;
    
    EVAL map.put("@NANOSECONDS@", Int(CEILING(c.nanoseconds)));
    EVAL map.put("@TIMESTEP@", Int(ROUND(c.timestep / 1.0d-12)) & "ps");
    EVAL map.put("@OPTIONS@", SimOptions[c.simu]);
    EVAL map.put("@CORNER@", TechCornNames[c.tech][c.corn]);

    CASE c.mode OF
      Mode.Dyn =>
      EVAL map.put("@RESET_SOURCE@",
                   "Vres _RESET 0 DC=0 PWL 0 0 10ns 0 10.1ns vtrue")
    |
      Mode.Leak =>
      EVAL map.put("@RESET_SOURCE@", "Vres _RESET 0 DC=0")
    END;

    WITH sufx = TechTranSufxs[c.tech][c.tran] DO
      IF sufx = NIL THEN
        Debug.Error(F("No mapping for %s in %s",
                      TranNames[c.tran],
                      TechNames[c.tech]))
      END;
      EVAL map.put("@TRANSUFX@", sufx)
    END;

    EVAL map.put("@TRANSIZE@", TechTranSizes[c.tech]);

    iter := extraMap.iterate();
    WHILE iter.next(k, v) DO
      WITH key = F("@%s@", k),
           hadIt = map.put(key, v) DO
        IF hadIt THEN
          Debug.Error("Duplicate mapping for key " & key)
        END
      END
    END;

    iter := overrideMap.iterate();
    WHILE iter.next(k, v) DO
      WITH key   = F("@%s@", k),
           <*NOWARN*>hadIt = map.put(key, v) DO
        (* skip *)
      END
    END
  END MapCommon;

PROCEDURE LoadTemplate(path : Pathname.T) : TextSeq.T
  RAISES { OSError.E, Rd.Failure } =
  VAR
    rd := FileRd.Open(path);
    res := NEW(TextSeq.T).init();
  BEGIN
    TRY
      LOOP
        WITH line = Rd.GetLine(rd) DO
          res.addhi(line)
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;
    Rd.Close(rd);
    RETURN res
  END LoadTemplate;

PROCEDURE WriteTemplate(template : TextSeq.T; path : Pathname.T)
  RAISES { OSError.E, Wr.Failure } =
  VAR
    wr := FileWr.Open(path);
  BEGIN
    FOR i := 0 TO template.size() - 1 DO
      Wr.PutText(wr, template.get(i));
      Wr.PutChar(wr, '\n')
    END;
    Wr.Close(wr)
  END WriteTemplate;

PROCEDURE ModifyTemplate(template : TextSeq.T; map : TextTextTbl.T) =
  VAR
    k, v, line : TEXT;
    iter := map.iterate();
  BEGIN
    WHILE iter.next(k, v) DO
      IF Verbose THEN
        Debug.Out(F("k %s -> v %s", k, Debug.UnNil(v)))
      END
    END;
    
    FOR i := 0 TO template.size() - 1 DO
      line := template.get(i);
      iter := map.iterate();
      WHILE iter.next(k, v) DO
        IF v = NIL THEN
          Debug.Error("NIL mapping for " & k)
        END;
        line := CitTextUtils.Replace(line, k, v)
      END;
      template.put(i, line)
    END
  END ModifyTemplate;

CONST DefSimRoot = "circuit";

PROCEDURE DoCommonSetup(VAR c : Config) =
  CONST
    DefaultTimeStep = 1.0d-12;
    MaxTimeSteps    = 50000.0d0;

  BEGIN
    WITH deltaV = c.volt - ApproxThresh[c.tran],
         stepsV = deltaV / 0.035d0,  (* kT/q *)
         threshDelayFactor = Math.exp(-stepsV),
         
         kelvinTemp      = c.temp - AbsZero,
         baseTemp        = 120.0d0 - AbsZero,
         tempDelayFactor = Math.pow(kelvinTemp / baseTemp, -1.5d0),
         cornDelayFactor = CornDelay[c.corn],
         delayFactor     = (1.0d0 + threshDelayFactor) * tempDelayFactor * cornDelayFactor,
         nanoseconds     = 10.0d0 +
                           ParaNanoFactor[c.para] * 10.0d0 * (delayFactor + 1.5d0),
         timestep        = MAX(DefaultTimeStep,
                               nanoseconds * 1.0d-9 / MaxTimeSteps)
     DO
      Debug.Out(F("tempDelayFactor %s, thresDelayFactor %s, delayFactor %s, nanoseconds %s",
                  LR(tempDelayFactor),
                  LR(threshDelayFactor),
                  LR(delayFactor),
                  LR(nanoseconds)));
      
      c.nanoseconds := nanoseconds;
      c.timestep := timestep
    END;
  END DoCommonSetup;

CONST ParaNanoFactor = ARRAY BOOLEAN OF LONGREAL { 1.0d0, 2.0d0 };
      
PROCEDURE DoSetup(READONLY c : Config) =
  VAR
    SimFile := c.simRoot & ".sp";
    map     := NEW(TextTextTbl.Default).init();
    template : TextSeq.T;
  BEGIN
    MapCommon(c, map);
    MapTech[c.tech](c, map);

    TRY
      template := LoadTemplate(c.templatePath);
    EXCEPT
      OSError.E(e) => Debug.Error(F("Couldn't open template file \"%s\" : OSError.E : %s",
                                    c.templatePath, AL.Format(e)))
    |
      Rd.Failure(e) =>
      Debug.Error(F("Couldn't read template file \"%s\" : Rd.Failure : %s",
                                    c.templatePath, AL.Format(e)))
    END;

    ModifyTemplate(template, map);

    TRY
      WriteTemplate(template, SimFile)
    EXCEPT
      OSError.E(e) => Debug.Error(F("Couldn't write simulation file \"%s\" : OSError.E : %s",
                                    SimFile, AL.Format(e)))
    |
      Wr.Failure(e) =>
      Debug.Error(F("Couldn't write simulation file \"%s\" : Wr.Failure : %s",
                    SimFile, AL.Format(e)))
    END
  END DoSetup;

TYPE
  MyCb = Watchdog.Callback OBJECT
    cmd : TEXT;
    wr  : TextWr.T;
  OVERRIDES
    do := MyCbDo;
  END;

PROCEDURE MyCbDo(cb : MyCb) =
  BEGIN
    Debug.Out(F("\n!!! WOOF WOOF !!!\nCommand \"%s\" with output\n====>\n%s\n<====\n\nWatchdog expired!  Exiting!",
                cb.cmd,
                TextWr.ToText(cb.wr)));
    Process.Exit(1)
  END MyCbDo;
  
PROCEDURE DoSimulate(READONLY c : Config) =
  <*FATAL OSError.E*> (* this pertains to the TextWr *)
  VAR
    wr := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);

    cmd := F("%s/xa %s.sp -o %s", c.xaPath, c.simRoot, c.simRoot);
  BEGIN
    
    (*Wr.Close(wrIn);*)
    CASE c.simu OF
      Simu.Xa =>
      Debug.Out("DoSimulate: " & cmd);
      WITH wd = NEW(Watchdog.T).init(ProcDeadline),
           c  = ProcUtils.RunText(cmd,
                                  stdout := stdout,
                                  stderr := stderr,
                                  stdin  := NIL),
           cb = NEW(MyCb, cmd := cmd, wr := wr) DO
        wd.setExpireAction(cb);
        TRY
          c.wait()
        EXCEPT
          ProcUtils.ErrorExit(err) =>
          Debug.Error(F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                        cmd,
                        TextWr.ToText(wr),
                        ProcUtils.FormatError(err)))
        END;
        wd.kill()
      END
    |
      Simu.Hspice => <*ASSERT FALSE*>
    END;
    Debug.Out("DoSimulate output :\n" & TextWr.ToText(wr))
  END DoSimulate;

PROCEDURE DoConvert(READONLY c : Config) =
  <*FATAL OSError.E*> (* this pertains to the TextWr *)
  VAR
    wr := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);

    cmd := F("/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/ct/AMD64_LINUX/ct -fsdb /nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/fsdb/src/nanosimrd -threads 4 -wthreads 1 -R %s %s.fsdb %s",
             LR(MAX(c.timestep, 50.0d-12)), c.simRoot, c.simRoot);
  BEGIN 
    (*Wr.Close(wrIn);*)
    CASE c.simu OF
      Simu.Xa =>
      Debug.Out("DoConvert: " & cmd);
      WITH wd = NEW(Watchdog.T).init(ProcDeadline),
           c = ProcUtils.RunText(cmd,
                                 stdout := stdout,
                                 stderr := stderr,
                                 stdin  := NIL),
           cb = NEW(MyCb, cmd := cmd, wr := wr) DO
        wd.setExpireAction(cb);
        TRY
          c.wait()
        EXCEPT
          ProcUtils.ErrorExit(err) =>
          Debug.Error(F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                        cmd,
                        TextWr.ToText(wr),
                        ProcUtils.FormatError(err)))
        END;
        wd.kill()
      END
    |
      Simu.Hspice => <*ASSERT FALSE*>
    END;
    Debug.Out("DoConvert output :\n" & TextWr.ToText(wr));
  END DoConvert;


PROCEDURE DoClean(READONLY c : Config) =
  BEGIN
    TRY FS.DeleteFile(F("%s.fsdb", c.simRoot)) EXCEPT ELSE END;

    TRY
      CONST
        CtWorkDir = "ct.work";
      VAR
        iter := FS.Iterate(CtWorkDir);
        fn : Pathname.T;
      BEGIN
        WHILE iter.next(fn) DO
          WITH ffn = CtWorkDir & "/" & fn DO
            IF Verbose THEN
              Debug.Out("Attempting to delete "& ffn)
            END;
            TRY FS.DeleteFile(ffn) EXCEPT ELSE END
          END
        END
      END;
      IF Verbose THEN
        Debug.Out("Attempting to delete "& "ct.work")
      END;
      FS.DeleteDirectory("ct.work")
    EXCEPT ELSE END
  END DoClean;
  
PROCEDURE DoMeasure(READONLY c : Config) =
  VAR
    trace : Trace.T;
    nSteps : CARDINAL;
    timeData, nodeData : REF ARRAY OF LONGREAL;
    
  BEGIN
    TRY
      trace := NEW(Trace.T).init(DefSimRoot);
    EXCEPT
      OSError.E(x) =>
      Debug.Error("OSError.E reading trace/names file : " & AL.Format(x))
    |
      Rd.Failure(x) =>
      Debug.Error("I/O error reading trace/names file : " & AL.Format(x))
    |
      Rd.EndOfFile =>
      Debug.Error("Short read reading trace/names file")
    END;

    nSteps := trace.getSteps();

    timeData := NEW(REF ARRAY OF LONGREAL, nSteps);
    nodeData := NEW(REF ARRAY OF LONGREAL, nSteps);

    TRY
      trace.getTimeData(timeData^);
    EXCEPT
      Rd.Failure, Rd.EndOfFile => Debug.Error("Trouble reading TIME data")
    END;
    
    Debug.Out(F("nSteps %s", Int(nSteps)));
    
    Debug.Out(F("first time %s, last time %s, step %s",
                LR(timeData[0]),
                LR(timeData[nSteps - 1]),
                LR(trace.getTimeStep())));
    
    CONST
      StartTime = 12.0d-9;
      StartTran = 1;
    VAR
      xIdx := GetIdx(trace, "x[0]");
      iIdx := GetIdx(trace, "vissx");
      cycle, meancurrent : LONGREAL;
    BEGIN
      TRY
        trace.getNodeData(xIdx, nodeData^);
      EXCEPT
        Rd.Failure, Rd.EndOfFile => Debug.Error("Trouble reading node data")
      END;
      
      cycle := CycleTime(timeData^, nodeData^,
                         c.volt / 2.0d0, StartTime, StartTran, StartTran + 1);
      
      Debug.Out("Measured cycle time " & LR(cycle));

      TRY
        trace.getNodeData(iIdx, nodeData^);
      EXCEPT
        Rd.Failure, Rd.EndOfFile => Debug.Error("Trouble reading node data")
      END;

      meancurrent := -1.0d0 / 1.0d6 *
          MeanValue(timeData^, nodeData^, StartTime);
      
      Debug.Out("Measured mean current " & LR(meancurrent));

      TRY
        VAR
          wr := FileWr.Open("measure.dat");
        BEGIN
          Wr.PutText(wr,
                     FN("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
                        ARRAY OF TEXT {
                           TechNames[c.tech],
                           CornNames[c.corn],
                           TranNames[c.tran],
                           TopoNames[c.topo],
                           ModeNames[c.mode],
                           SimuNames[c.simu],
                           LR(c.volt),
                           LR(c.temp),
                           LR(cycle),
                           LR(meancurrent)
                           }));
          Wr.Close(wr)
        END
      EXCEPT
        OSError.E(x) =>
        Debug.Error(F("Couldn't open measurement output file : OSError.E : %s",
                      AL.Format(x)))
      |
        Wr.Failure(x) =>
        Debug.Error(F("Couldn't write measurement output file : Wr.Failure : %s",
                      AL.Format(x)))
      END
    END
  END DoMeasure;

PROCEDURE CycleTime(READONLY timea, nodea : ARRAY OF LONGREAL;
                    cross                 : LONGREAL;
                    startTime             : LONGREAL;
                    startTran, endTran    : CARDINAL) : LONGREAL =
  (* looking at nodea values beyond startTime, 
     find the startTran and endTran rising transitions
     and return the average cycle time in that range *)
  VAR
    pn := nodea[FIRST(nodea)];
    pt : LONGREAL;
    seq := NEW(LongRealSeq.T).init();
  BEGIN
    FOR i := FIRST(timea) TO LAST(timea) DO
      WITH t = timea[i],
           n = nodea[i] DO
        IF t >= startTime THEN
          IF n > cross AND pn <= cross THEN
            WITH delV  = n - pn,
                 delT  = t - pt,
                 delV0 = cross - pn,
                 delT0 = delV0 / delV * delT DO
              seq.addhi(pt + delT0)
            END
          END
        END;
        pn := n;
        pt := t
      END
    END;

    IF endTran < seq.size() THEN
      WITH cnt = endTran - startTran,
           sT  = seq.get(startTran),
           eT  = seq.get(endTran),
           res = (eT - sT) / FLOAT(cnt, LONGREAL) DO
        RETURN res
      END
    ELSE
      RETURN LAST(LONGREAL)
    END
  END CycleTime;

PROCEDURE MeanValue(READONLY timea, nodea : ARRAY OF LONGREAL;
                    startTime             : LONGREAL) : LONGREAL =
  (* looking at nodea values beyond startTime, 
     return the mean value in the rest of history *)
  VAR
    sum := 0.0d0;
    cnt := 0;
  BEGIN
    FOR i := FIRST(timea) TO LAST(timea) DO
      WITH t = timea[i],
           n = nodea[i] DO
        IF t >= startTime THEN
          sum := sum + n;
          INC(cnt)
        END
      END
    END;

    RETURN sum / FLOAT(cnt, LONGREAL)
  END MeanValue;

PROCEDURE GetIdx(trace : Trace.T; of : TEXT) : CARDINAL =
  VAR
    res : CARDINAL;
    hadIt := trace.getNodeIdx(of, res);
  BEGIN
    IF NOT hadIt THEN
      Debug.Error(F("GetIdx : \"%s\" not found", of))
    END;
    RETURN res
  END GetIdx;

TYPE
  Config = RECORD
    tech : Tech;
    tran : Tran;
    mode : Mode;
    simu : Simu;
    topo : Topo;
    corn : Corn;
    volt := 0.0d0;
    temp := 0.0d0;
    nanoseconds : LONGREAL; (* length of sim in ns *)
    timestep : LONGREAL; (* in seconds *)
    
    workDir : Pathname.T;
    createWorkDir : BOOLEAN;
    templatePath : Pathname.T;
    phazz := SET OF Phaz { Phaz.Setup };
    hspiceModelRoot : Pathname.T;
    hspiceModel     : Pathname.T;

    hspiceLibModels : Pathname.T :=
        "/p/hdk/cad/pdk/pdk764_r0.4HP3_22ww20.1/cmi/hspice/cmi/lnx86/64bit";
    (* what is this file? *)

    pdmiLib         : Pathname.T;
    simRoot := DefSimRoot;
    xaPath : Pathname.T := "/p/hdk/cad/xa/S-2021.09-SP2//bin/";

    para : BOOLEAN; (* parasitic simulation yes/no *)
  END;

VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  c : Config;
  extraMap, overrideMap := NEW(TextTextTbl.Default).init();
  
BEGIN
  TRY
    c.createWorkDir := pp.keywordPresent("-C");
    
    IF pp.keywordPresent("-tech") THEN
      c.tech := VAL(Lookup(pp.getNext(), TechNames), Tech);
      c.hspiceModel := TechHspiceModels[c.tech];
      c.hspiceModelRoot := TechHspiceModelRoots[c.tech];
    END;

    IF pp.keywordPresent("-para") THEN
      WITH arg = pp.getNext() DO
        TRY
          c.para := Scan.Bool(arg)
        EXCEPT
          Lex.Error =>
          Debug.Error(F("Lex.Error : -para arg %s not a boolean", arg))
        END
      END
    END;

    IF c.para THEN
      ProcDeadline := ProcDeadline * ParasiticDeadlineMultiplier
    END;

    IF pp.keywordPresent("-tran") THEN
      c.tran := VAL(Lookup(pp.getNext(), TranNames), Tran)
    END;

    IF pp.keywordPresent("-volt") THEN
      c.volt := pp.getNextLongReal()
    END;
    
    IF pp.keywordPresent("-temp") THEN
      c.temp := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-mode") THEN
      c.mode := VAL(Lookup(pp.getNext(), ModeNames), Mode)
    END;

    IF pp.keywordPresent("-simu") THEN
      c.simu := VAL(Lookup(pp.getNext(), SimuNames), Simu)
    END;

    IF pp.keywordPresent("-topo") THEN
      c.topo := VAL(Lookup(pp.getNext(), TopoNames), Topo)
    END;

    IF pp.keywordPresent("-corn") THEN
      c.corn := VAL(Lookup(pp.getNext(), CornNames), Corn)
    END;

    IF pp.keywordPresent("-d") THEN
      c.workDir := pp.getNext()
    END;

    IF pp.keywordPresent("-T") THEN
      c.templatePath := pp.getNext()
    END;

    IF pp.keywordPresent("-deadline") THEN
      ProcDeadline := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-p") THEN
      c.phazz := SET OF Phaz {};
      
      REPEAT
        c.phazz := c.phazz + SET OF Phaz { VAL(Lookup(pp.getNext(), PhazNames),
                                           Phaz) }
      UNTIL NOT pp.keywordPresent("-p")
    END;

    IF pp.keywordPresent("-all") THEN
      c.phazz := SET OF Phaz { FIRST(Phaz) .. LAST(Phaz) }
    END;

    WHILE pp.keywordPresent("-m") DO
      WITH kk = pp.getNext(),
           vv = pp.getNext() DO
        IF extraMap.put(kk, vv) THEN
          Debug.Error("Multiple mappings for key " & vv)
        END
      END
    END;

    WHILE pp.keywordPresent("-O") DO
      WITH kk = pp.getNext(),
           vv = pp.getNext() DO
        EVAL overrideMap.put(kk, vv)
      END
    END;
    
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF c.workDir # NIL THEN
    TRY
      IF c.createWorkDir THEN
        TRY FS.CreateDirectory(c.workDir) EXCEPT ELSE END
      END;
      Process.SetWorkingDirectory(c.workDir)
    EXCEPT
      OSError.E(e) =>
      Debug.Error(F("Couldn't set working directory to \"%s\" : OSError.E : %s",
                    c.workDir, AL.Format(e)))
    END
  END;

  DoCommonSetup(c);
  
  FOR phaz := FIRST(Phaz) TO LAST(Phaz) DO
    IF phaz IN c.phazz THEN
      Debug.Out(F("*****  PHASE %s  ***** ", PhazNames[phaz]));
      Phases[phaz](c)
    END
  END
  
END Main.
