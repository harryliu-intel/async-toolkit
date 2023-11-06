INTERFACE P1278p3TechProcess;

IMPORT TechProcess;
FROM TechConfig  IMPORT Tran, Corn, Gate;
FROM TechProcess IMPORT TranSufxs;
IMPORT Pathname;

TYPE
  HSpiceModels     = { R0p5, R0p8, R0p9e_beta, AsFit2023WW29, R0p9eu1 };

  Stdcells         = { i0s, i0m };

  (* note that this file below refers only to the i0s library, i0m is 
     maintained elsewhere *)

CONST
  StdcellNames     = ARRAY Stdcells OF TEXT { "i0s", "i0m" };

  TranFlavor       = ARRAY Stdcells OF TEXT { "b", "a" }; (* transistor flavor *)
  
CONST
  HSpiceModelNames = ARRAY HSpiceModels OF TEXT { "0p5", "0p8", "0p9e_beta", "asfit2023ww29", "0p9eu1" };

  HSpiceModelRoots = ARRAY HSpiceModels OF Pathname.T {
  "/p/hdk/cad/pdk/pdk783_r0.5_22ww52.5/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp",
  "/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/models/core/hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp",
  "/p/hdk/cad/pdk/pdk783_r0.9e_23ww29.2_beta/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp",
  "/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/1278_lowvoltage/2023ww29d2/models_core_hspice/1/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp",
  "/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/p1278_3x0p9eu1/2023ww43d5/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp"
  };

CONST
  P = TechProcess.T {
  tranSufxs :=TranSufxs { NIL,       (* elvt *)
                "hpbulvt", (* ulvt *)
                NIL,       (* ulvtll *)
                "hpblvt",  (* lvt *)
                NIL,       (* lvtll *)
                "hpbsvt",  (* svt *)
                "hpbhvt"   (* hvt = svtll *)
  }

      ,
  tranSize :="w=2 l=14e-9 m=1 nf=1",

  hspiceModel := "p1278_3.hsp",

  hspiceModelRoot := HSpiceModelRoots[HSpiceModels.R0p5],

  cornNames :=ARRAY Corn OF TEXT {
  "tttt",
  "ss",
  "ff",
  "sf",
  "fs"
  },

  cellPaths :=ARRAY Gate OF ARRAY Tran OF TEXT {
    XorPaths,
    XorPaths,
    BufPaths,
    AoiCellPaths,
    OaiCellPaths,
    Xor1Paths,
    Xor1Paths,
    Xor2Paths,
    Xor2Paths,

    Aoi1Paths,
    Aoi1Paths,
    Aoi2Paths,
    Aoi2Paths,

    Oai1Paths,
    Oai1Paths,
    Oai2Paths,
    Oai2Paths,

    Xor1Paths,
    Xor2Paths,
    Xor3Paths
  },

  cellNames :=ARRAY Gate OF ARRAY Tran OF TEXT {
    XorCellNames,
    XorCellNames,
    BufCellNames,
    AoiCellNames,
    OaiCellNames,
    Xor1CellNames,
    Xor1CellNames,
    Xor2CellNames,
    Xor2CellNames,

    Aoi1CellNames,
    Aoi1CellNames,
    Aoi2CellNames,
    Aoi2CellNames,

    Oai1CellNames,
    Oai1CellNames,
    Oai2CellNames,
    Oai2CellNames,

    Xor1CellNames,
    Xor2CellNames,
    Xor3CellNames
},

  plugText :=""

      };

  StdCellRoot = "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v2p0_efv";

  StdCellUlvtRoot = StdCellRoot & "/base_ulvt/spf/lib783_i0s_160h_50pp_base_ulvt_100c_tttt_ctyp/";
  StdCellLvtRoot = StdCellRoot & "/base_lvt/spf/lib783_i0s_160h_50pp_base_lvt_100c_tttt_ctyp/";
  StdCellSvtRoot = StdCellRoot & "/base_svt/spf/lib783_i0s_160h_50pp_base_svt_100c_tttt_ctyp/";
  StdCellHvtRoot = StdCellRoot & "/base_hvt/spf/lib783_i0s_160h_50pp_base_hvt_100c_tttt_ctyp/";

  (* Z1 paths *)
  
  StdCellUlvt1Root = StdCellRoot & "/ldrbase_ulvt/spf/lib783_i0s_160h_50pp_ldrbase_ulvt_100c_tttt_ctyp/";
  StdCellLvt1Root = StdCellRoot & "/ldrbase_lvt/spf/lib783_i0s_160h_50pp_ldrbase_lvt_100c_tttt_ctyp/";
  StdCellSvt1Root = StdCellRoot & "/ldrbase_svt/spf/lib783_i0s_160h_50pp_ldrbase_svt_100c_tttt_ctyp/";
  StdCellHvt1Root = StdCellRoot & "/ldrbase_hvt/spf/lib783_i0s_160h_50pp_ldrbase_hvt_100c_tttt_ctyp/";

  (* xor (only) paths *)
  XorStdCellUlvt1Root = StdCellRoot & "/ldrdsibase_ulvt/spf/lib783_i0s_160h_50pp_ldrdsibase_ulvt_100c_tttt_ctyp/";
  XorStdCellLvt1Root = StdCellRoot & "/ldrdsibase_lvt/spf/lib783_i0s_160h_50pp_ldrdsibase_lvt_100c_tttt_ctyp/";
  XorStdCellSvt1Root = StdCellRoot & "/ldrdsibase_svt/spf/lib783_i0s_160h_50pp_ldrdsibase_svt_100c_tttt_ctyp/";
  XorStdCellHvt1Root = StdCellRoot & "/ldrdsibase_hvt/spf/lib783_i0s_160h_50pp_ldrdsibase_hvt_100c_tttt_ctyp/";
    
  XorStdCellUlvt2Root = StdCellRoot & "/dsibase_ulvt/spf/lib783_i0s_160h_50pp_dsibase_ulvt_100c_tttt_ctyp/";
  XorStdCellLvt2Root = StdCellRoot & "/dsibase_lvt/spf/lib783_i0s_160h_50pp_dsibase_lvt_100c_tttt_ctyp/";
  XorStdCellSvt2Root = StdCellRoot & "/dsibase_svt/spf/lib783_i0s_160h_50pp_dsibase_svt_100c_tttt_ctyp/";
  XorStdCellHvt2Root = StdCellRoot & "/dsibase_hvt/spf/lib783_i0s_160h_50pp_dsibase_hvt_100c_tttt_ctyp/";
    
  XorStdCellUlvt3Root = XorStdCellUlvt2Root;
  XorStdCellLvt3Root  = XorStdCellLvt2Root;
  XorStdCellSvt3Root  = XorStdCellSvt2Root;
  XorStdCellHvt3Root = XorStdCellHvt2Root;
  

  BufPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvtRoot & "i0sbfn000aa1n02x5.spf",
    NIL,
    StdCellLvtRoot & "i0sbfn000ab1n02x5.spf",
    NIL,
    StdCellSvtRoot & "i0sbfn000ac1n02x5.spf",
    StdCellHvtRoot & "i0sbfn000ad1n02x5.spf"
  };
  
  BufCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0sbfn000aa1n02x5",
    NIL,
    "i0sbfn000ab1n02x5",
    NIL,
    "i0sbfn000ac1n02x5",
    "i0sbfn000ad1n02x5"
  };

  XorPaths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvt2Root & "i0sxor002aa1n02x5.spf",
    NIL,
    XorStdCellLvt2Root & "i0sxor002ab1n02x5.spf",
    NIL,
    XorStdCellSvt2Root & "i0sxor002ac1n02x5.spf",
    XorStdCellHvt2Root & "i0sxor002ad1n02x5.spf"
  };

  Xor1Paths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvt1Root & "i0sxor002aa1n01x1.spf",
    NIL,
    XorStdCellLvt1Root & "i0sxor002ab1n01x1.spf",
    NIL,
    XorStdCellSvt1Root & "i0sxor002ac1n01x1.spf",
    XorStdCellHvt1Root & "i0sxor002ad1n01x1.spf"
  };
  
  Xor2Paths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvt2Root & "i0sxor002aa1n02x5.spf",
    NIL,
    XorStdCellLvt2Root & "i0sxor002ab1n02x5.spf",
    NIL,
    XorStdCellSvt2Root & "i0sxor002ac1n02x5.spf",
    XorStdCellHvt2Root & "i0sxor002ad1n02x5.spf"
    
  };
  
  Xor3Paths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvt3Root & "i0sxor002aa1n03x5.spf",
    NIL,
    XorStdCellLvt3Root & "i0sxor002ab1n03x5.spf",
    NIL,
    XorStdCellSvt3Root & "i0sxor002ac1n03x5.spf",
    XorStdCellHvt3Root & "i0sxor002ad1n03x5.spf"
    
  };
  
  XorCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0sxor002aa1n02x5",
    NIL,
    "i0sxor002ab1n02x5",
    NIL,
    "i0sxor002ac1n02x5",
    "i0sxor002ad1n02x5"
  };

  Xor3CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0sxor002aa1n03x5",
    NIL,
    "i0sxor002ab1n03x5",
    NIL,
    "i0sxor002ac1n03x5",
    "i0sxor002ad1n03x5"
  };

  Xor1CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0sxor002aa1n01x1",
    NIL,
    "i0sxor002ab1n01x1",
    NIL,
    "i0sxor002ac1n01x1",
    "i0sxor002ad1n01x1"
  };

  Xor2CellNames = XorCellNames;

  AoiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0saoi012aa1n02x5",
    NIL,
    "i0saoi012ab1n02x5",
    NIL,
    "i0saoi012ac1n02x5",
    "i0saoi012ad1n02x5"
  };

  Aoi1CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0saoi012aa1n01x1",
    NIL,
    "i0saoi012ab1n01x1",
    NIL,
    "i0saoi012ac1n01x1",
    "i0saoi012ad1n01x1"
  };

  Aoi2CellNames = AoiCellNames;

  OaiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0soai012aa1n02x5",
    NIL,
    "i0soai012ab1n02x5",
    NIL,
    "i0soai012ac1n02x5",
    "i0soai012ad1n02x5"
  };

  Oai1CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0soai012aa1n01x1",
    NIL,
    "i0soai012ab1n01x1",
    NIL,
    "i0soai012ac1n01x1",
    "i0soai012ad1n01x1"
  };

  Oai2CellNames = OaiCellNames;

  AoiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvtRoot & "i0saoi012aa1n02x5.spf",
    NIL,
    StdCellLvtRoot & "i0saoi012ab1n02x5.spf",
    NIL,
    StdCellSvtRoot & "i0saoi012ac1n02x5.spf",
    StdCellHvtRoot & "i0saoi012ad1n02x5.spf"
  };

  Aoi1Paths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvt1Root & "i0saoi012aa1n01x1.spf",
    NIL,
    StdCellLvt1Root & "i0saoi012ab1n01x1.spf",
    NIL,
    StdCellSvt1Root & "i0saoi012ac1n01x1.spf",
    StdCellHvt1Root & "i0saoi012ad1n01x1.spf"
  };

  Aoi2Paths = AoiCellPaths;

  OaiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvtRoot & "i0soai012aa1n02x5.spf",
    NIL,
    StdCellLvtRoot & "i0soai012ab1n02x5.spf",
    NIL,
    StdCellSvtRoot & "i0soai012ac1n02x5.spf",
    StdCellHvtRoot & "i0soai012ad1n02x5.spf"
  };

  Oai1Paths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvt1Root & "i0soai012aa1n01x1.spf",
    NIL,
    StdCellLvt1Root & "i0soai012ab1n01x1.spf",
    NIL,
    StdCellSvt1Root & "i0soai012ac1n01x1.spf",
    StdCellHvt1Root & "i0soai012ad1n01x1.spf"
  };

  Oai2Paths = OaiCellPaths;
  
CONST
  Brand = "P1278p3TechProcess";

END P1278p3TechProcess.
                                


      
