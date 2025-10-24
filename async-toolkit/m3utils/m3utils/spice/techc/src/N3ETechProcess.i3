(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE N3ETechProcess;

IMPORT TechProcess;
IMPORT N5TechProcess;

FROM TechConfig  IMPORT Tran, Gate;
FROM TechProcess IMPORT TranSufxs;

CONST
  P = TechProcess.T {
  tranSufxs := TranSufxs { "ch_elvt_mac",
                "ch_ulvt_mac",
                "ch_ulvtll_mac",
                "ch_lvt_mac",
                "ch_lvtll_mac",
                "ch_svt_mac",
                NIL
  },
  tranSize := "l=3n nfin=2 ppitch=0 fbound=262",

  hspiceModel :="cln3e_1d2_sp_v0d5_2p2_usage.l",


  hspiceModelRoot :="/p/tech1/n3e/tech-release/v0.5.0/models/1P17M_1Xa_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_6Y_hvhvhv_2Yy2R_shdmim_ut-alrdl/hspice",

  cornNames := N5TechProcess.P.cornNames,

  cellPaths :=ARRAY Gate OF ARRAY Tran OF TEXT {
    BufPaths,
    XorAltPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    BufPaths,
    Paths117,
    Paths117,
    Paths117,
    Paths117,
    Paths117,
    Paths117,
    Paths117,
    Paths117,
    NilPaths,
    NilPaths,
    NilPaths,
    NilPaths,
    NilPaths,
    NilPaths,
    NilPaths
  },

  cellNames :=ARRAY Gate OF ARRAY Tran OF TEXT {
    XorCellNames,
    XorCellNames,
    BufCellNames,
    AoiCellNames,
    OaiCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    AoiZ1Names,
    AoiZ1Names,
    AoiZ2Names,
    AoiZ2Names,
    OaiZ1Names,
    OaiZ1Names,
    OaiZ2Names,
    OaiZ2Names,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames
  },

  plugText :="vcc vssx"

      };

CONST
  XorAltPaths = ARRAY Tran OF TEXT {
  NIL,
  "/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/XOR2D1BWP169H3P48CPDULVTfix.spf",
  NIL,
  NIL,
  NIL,
  NIL,
  NIL
  };
  
  BufPaths = ARRAY Tran OF TEXT {
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_elvt_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_elvt_090b/tcbn03e_bwph169l3p48cpd_base_elvt_090b_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_ulvt_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_ulvt_090b/tcbn03e_bwph169l3p48cpd_base_ulvt_090b_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_ulvtll_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_ulvtll_090b/tcbn03e_bwph169l3p48cpd_base_ulvtll_090b_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_lvt_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_lvt_090b/tcbn03e_bwph169l3p48cpd_base_lvt_090b_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_lvtll_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_lvtll_090b/tcbn03e_bwph169l3p48cpd_base_lvtll_090b_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v0.9.0p3/tcbn03e_bwph169l3p48cpd_base_svt_lib/lpe_spice/tcbn03e_bwph169l3p48cpd_base_svt_090b/tcbn03e_bwph169l3p48cpd_base_svt_090b_lpe_typical_125c.spi",
  NIL
  };

  Paths117 = ARRAY Tran OF TEXT {
  (* 117 high alternating cells *)
  "/p/tech1/n3e/tech-release/v1.0.2/tcbn03e_bwp143mh117l3p48cpd_base_elvt_lib/lpe_spice/tcbn03e_bwp143mh117l3p48cpd_base_elvt_100a/tcbn03e_bwp143mh117l3p48cpd_base_elvt_100a_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v1.0.2/tcbn03e_bwp143mh117l3p48cpd_base_ulvt_lib/lpe_spice/tcbn03e_bwp143mh117l3p48cpd_base_ulvt_100a/tcbn03e_bwp143mh117l3p48cpd_base_ulvt_100a_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v1.0.2/tcbn03e_bwp143mh117l3p48cpd_base_ulvtll_lib/lpe_spice/tcbn03e_bwp143mh117l3p48cpd_base_ulvtll_100a/tcbn03e_bwp143mh117l3p48cpd_base_ulvtll_100a_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v1.0.2/tcbn03e_bwp143mh117l3p48cpd_base_lvt_lib/lpe_spice/tcbn03e_bwp143mh117l3p48cpd_base_lvt_100a/tcbn03e_bwp143mh117l3p48cpd_base_lvt_100a_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v1.0.2/tcbn03e_bwp143mh117l3p48cpd_base_lvtl_lib/lpe_spice/tcbn03e_bwp143mh117l3p48cpd_base_lvtl_100a/tcbn03e_bwp143mh117l3p48cpd_base_lvtl_100a_lpe_typical_125c.spi",
  "/p/tech1/n3e/tech-release/v1.0.2/tcbn03e_bwp143mh117l3p48cpd_base_svt_lib/lpe_spice/tcbn03e_bwp143mh117l3p48cpd_base_svt_100a/tcbn03e_bwp143mh117l3p48cpd_base_svt_100a_lpe_typical_125c.spi",
  NIL
  };

  NilPaths  = ARRAY Tran OF TEXT { NIL, NIL, NIL, NIL, NIL, NIL, NIL };

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
  "BUFFD1BWP169H3P48CPDELVT",
  "BUFFD1BWP169H3P48CPDULVT",
  "BUFFD1BWP169H3P48CPDULVTLL",
  "BUFFD1BWP169H3P48CPDLVT",
  "BUFFD1BWP169H3P48CPDLVTLL",
  "BUFFD1BWP169H3P48CPDSVT",
  NIL
  };

  XorCellNames = ARRAY Tran OF TEXT {
  "XOR2D1BWP169H3P48CPDELVT",
  "XOR2D1BWP169H3P48CPDULVT",
  "XOR2D1BWP169H3P48CPDULVTLL",
  "XOR2D1BWP169H3P48CPDLVT",
  "XOR2D1BWP169H3P48CPDLVTLL",
  "XOR2D1BWP169H3P48CPDSVT",
  NIL
  };
  
  OaiCellNames = ARRAY Tran OF TEXT {
  "OAI21D1BWP169H3P48CPDELVT",
  "OAI21D1BWP169H3P48CPDULVT",
  "OAI21D1BWP169H3P48CPDULVTLL",
  "OAI21D1BWP169H3P48CPDLVT",
  "OAI21D1BWP169H3P48CPDLVTLL",
  "OAI21D1BWP169H3P48CPDSVT",
  NIL
  };

  AoiCellNames = ARRAY Tran OF TEXT {
  "AOI21D1BWP169H3P48CPDELVT",
  "AOI21D1BWP169H3P48CPDULVT",
  "AOI21D1BWP169H3P48CPDULVTLL",
  "AOI21D1BWP169H3P48CPDLVT",
  "AOI21D1BWP169H3P48CPDLVTLL",
  "AOI21D1BWP169H3P48CPDSVT",
  NIL
  };

  AoiZ1Names = ARRAY Tran OF TEXT {
  "AOI21D0P5BWP143M117H3P48CPDELVT",
  "AOI21D0P5BWP143M117H3P48CPDULVT",
  "AOI21D0P5BWP143M117H3P48CPDULVTLL",
  "AOI21D0P5BWP143M117H3P48CPDLVT",
  "AOI21D0P5BWP143M117H3P48CPDLVTLL",
  "AOI21D0P5BWP143M117H3P48CPDSVT",
  NIL
  };
  
  AoiZ2Names = ARRAY Tran OF TEXT {
  "AOI21D1BWP143M117H3P48CPDELVT",
  "AOI21D1BWP143M117H3P48CPDULVT",
  "AOI21D1BWP143M117H3P48CPDULVTLL",
  "AOI21D1BWP143M117H3P48CPDLVT",
  "AOI21D1BWP143M117H3P48CPDLVTLL",
  "AOI21D1BWP143M117H3P48CPDSVT",
  NIL
  };
  
  OaiZ1Names = ARRAY Tran OF TEXT {
  "OAI21D0P5BWP143M117H3P48CPDELVT",
  "OAI21D0P5BWP143M117H3P48CPDULVT",
  "OAI21D0P5BWP143M117H3P48CPDULVTLL",
  "OAI21D0P5BWP143M117H3P48CPDLVT",
  "OAI21D0P5BWP143M117H3P48CPDLVTLL",
  "OAI21D0P5BWP143M117H3P48CPDSVT",
  NIL
  };
  
  OaiZ2Names = ARRAY Tran OF TEXT {
  "OAI21D1BWP143M117H3P48CPDELVT",
  "OAI21D1BWP143M117H3P48CPDULVT",
  "OAI21D1BWP143M117H3P48CPDULVTLL",
  "OAI21D1BWP143M117H3P48CPDLVT",
  "OAI21D1BWP143M117H3P48CPDLVTLL",
  "OAI21D1BWP143M117H3P48CPDSVT",
  NIL
  };
  
CONST
  Brand = "N3ETechProcess";

END N3ETechProcess.
                                


      
