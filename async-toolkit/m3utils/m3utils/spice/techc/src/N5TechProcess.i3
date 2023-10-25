INTERFACE N5TechProcess;
IMPORT TechProcess;
FROM TechConfig  IMPORT Tran, Corn, Gate;
FROM TechProcess IMPORT TranSufxs;

CONST
  P = TechProcess.T {
    tranSufxs := TranSufxs { "ch_elvt_mac",
                             "ch_ulvt_mac",
                             "ch_ulvtll_mac",
                             "ch_lvt_mac",
                             "ch_lvtll_mac",
                             "ch_svt_mac",
                             "ch_svtll_mac"
  },
    tranSize := "l=6n nfin=2 ppitch=0 fbound=9",
    hspiceModel := "cln5_1d2_sp_v1d1_2p2_usage.l",
    hspiceModelRoot := "/p/tech/n5/tech-release/v1.1.3/models/1P15M_1X_h_1Xb_v_1Xe_h_1Ya_v_1Yb_h_5Y_vhvhv_2Yy2R/hspice",

    cornNames := ARRAY Corn OF TEXT {
  "TTGlobalCorner_LocalMC_MOS_MOSCAP",
  "SSGlobalCorner_LocalMC_MOS_MOSCAP",
  "FFGlobalCorner_LocalMC_MOS_MOSCAP",
  "SFGlobalCorner_LocalMC_MOS_MOSCAP",
  "FSGlobalCorner_LocalMC_MOS_MOSCAP"
  },
    cellPaths := ARRAY Gate OF ARRAY Tran OF TEXT {
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    NilPaths,
    NilPaths,
    NilPaths
  },

    cellNames := ARRAY Gate OF ARRAY Tran OF TEXT {
    XorCellNames,
    XorCellNames,
    BufCellNames,
    AoiCellNames,
    OaiCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    AoiCellNames,  (* should be Z1 *)
    AoiCellNames,  (* should be Z1 *)
    AoiCellNames,  (* should be Z2 *)
    AoiCellNames,  (* should be Z2 *)
    OaiCellNames,  (* should be Z1 *)
    OaiCellNames,  (* should be Z1 *)
    OaiCellNames,  (* should be Z2 *)
    OaiCellNames,  (* should be Z2 *)
    NilCellNames,
    NilCellNames,
    NilCellNames
  },
    plugText := "vcc vssx"
  };

  NilPaths  = ARRAY Tran OF TEXT { NIL, NIL, NIL, NIL, NIL, NIL, NIL };

  BufPaths = ARRAY Tran OF TEXT {
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_elvt_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_elvt_090a/tcbn05_bwph210l6p51cnod_base_elvt_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_ulvt_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_ulvt_090a/tcbn05_bwph210l6p51cnod_base_ulvt_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_ulvtll_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_ulvtll_090a/tcbn05_bwph210l6p51cnod_base_ulvtll_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_lvt_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_lvt_090a/tcbn05_bwph210l6p51cnod_base_lvt_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_lvtll_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_lvtll_090a/tcbn05_bwph210l6p51cnod_base_lvtll_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_svt_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_svt_090a/tcbn05_bwph210l6p51cnod_base_svt_090a_lpe_typical_125c.spi",
  "/p/tech/n5/tech-prerelease/.dr/tcbn05_bwph210l6p51cnod_base_svtll_lib/v0.9.0_pre.1/lpe_spice/tcbn05_bwph210l6p51cnod_base_svtll_090a/tcbn05_bwph210l6p51cnod_base_svtll_090a_lpe_typical_125c.spi" };

  NilCellNames = ARRAY Tran OF TEXT {
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL
  };

  BufCellNames = ARRAY Tran OF TEXT {
  "BUFFD1BWP210H6P51CNODELVT",
  "BUFFD1BWP210H6P51CNODULVT",
  "BUFFD1BWP210H6P51CNODULVTLL",
  "BUFFD1BWP210H6P51CNODLVT",
  "BUFFD1BWP210H6P51CNODLVTLL",
  "BUFFD1BWP210H6P51CNODSVT",
  "BUFFD1BWP210H6P51CNODSVTLL"
  };

  XorCellNames = ARRAY Tran OF TEXT {
  "XOR2D1BWP210H6P51CNODELVT",
  "XOR2D1BWP210H6P51CNODULVT",
  "XOR2D1BWP210H6P51CNODULVTLL",
  "XOR2D1BWP210H6P51CNODLVT",
  "XOR2D1BWP210H6P51CNODLVTLL",
  "XOR2D1BWP210H6P51CNODSVT",
  "XOR2D1BWP210H6P51CNODSVTLL"
  };
  
  OaiCellNames = ARRAY Tran OF TEXT {
  "OAI21D1BWP210H6P51CNODELVT",
  "OAI21D1BWP210H6P51CNODULVT",
  "OAI21D1BWP210H6P51CNODULVTLL",
  "OAI21D1BWP210H6P51CNODLVT",
  "OAI21D1BWP210H6P51CNODLVTLL",
  "OAI21D1BWP210H6P51CNODSVT",
  "OAI21D1BWP210H6P51CNODSVTLL"
  };

  AoiCellNames = ARRAY Tran OF TEXT {
  "AOI21D1BWP210H6P51CNODELVT",
  "AOI21D1BWP210H6P51CNODULVT",
  "AOI21D1BWP210H6P51CNODULVTLL",
  "AOI21D1BWP210H6P51CNODLVT",
  "AOI21D1BWP210H6P51CNODLVTLL",
  "AOI21D1BWP210H6P51CNODSVT",
  "AOI21D1BWP210H6P51CNODSVTLL"
  };

CONST Brand = "N5TechProcess";

END N5TechProcess.
