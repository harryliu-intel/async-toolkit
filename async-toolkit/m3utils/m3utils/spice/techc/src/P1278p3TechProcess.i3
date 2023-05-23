INTERFACE P1278p3TechProcess;

IMPORT TechProcess;
FROM TechConfig  IMPORT Tran, Corn, Gate;
FROM TechProcess IMPORT TranSufxs;

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


  hspiceModelRoot :="/p/hdk/cad/pdk/pdk783_r0.5_22ww52.5/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp",

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
    Xor2Paths
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
    Xor2CellNames
  },

  plugText :=""

      };

  StdCellRoot = "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk050_r3v2p0_efv";

  StdCellUlvtRoot = StdCellRoot & "/base_ulvt/spf/lib783_i0s_160h_50pp_base_ulvt_100c_tttt_cmax/";
  StdCellLvtRoot = StdCellRoot & "/base_lvt/spf/lib783_i0s_160h_50pp_base_lvt_100c_tttt_cmax/";
  StdCellSvtRoot = StdCellRoot & "/base_svt/spf/lib783_i0s_160h_50pp_base_svt_100c_tttt_cmax/";
  StdCellHvtRoot = StdCellRoot & "/base_hvt/spf/lib783_i0s_160h_50pp_base_hvt_100c_tttt_cmax/";

  StdCellUlvt1Root = StdCellRoot & "/ldrdsibase_ulvt/spf/lib783_i0s_160h_50pp_ldrdsibase_ulvt_100c_tttt_ctyp/";
  StdCellLvt1Root = StdCellRoot & "/ldrdsibase_lvt/spf/lib783_i0s_160h_50pp_ldrdsibase_lvt_100c_tttt_ctyp/";
    
  StdCellUlvt2Root = StdCellRoot & "/dsibase_ulvt/spf/lib783_i0s_160h_50pp_dsibase_ulvt_100c_tttt_ctyp/";
  StdCellLvt2Root = StdCellRoot & "/dsibase_lvt/spf/lib783_i0s_160h_50pp_dsibase_lvt_100c_tttt_ctyp/";
    
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
    StdCellUlvtRoot & "i0sxor002aa1n02x5.spf",
    NIL,
    StdCellLvtRoot & "i0sxor002ab1n02x5.spf",
    NIL,
    StdCellSvtRoot & "i0sxor002ac1n02x5.spf",
    StdCellHvtRoot & "i0sxor002ad1n02x5.spf"
  };

  Xor1Paths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvt1Root & "i0sxor002aa1n01x1.spf",
    NIL,
    StdCellLvt1Root & "i0sxor002ab1n01x1.spf",
    NIL,
    NIL,
    NIL
  };
  
  Xor2Paths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvt2Root & "i0sxor002aa1n02x5.spf",
    NIL,
    StdCellLvt2Root & "i0sxor002ab1n02x5.spf",
    NIL,
    NIL,
    NIL
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

  Xor1CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0sxor002aa1n01x1",
    NIL,
    "i0sxor002ab1n01x1",
    NIL,
    NIL,
    NIL
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

  OaiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0soai012aa1n02x5",
    NIL,
    "i0soai012ab1n02x5",
    NIL,
    "i0soai012ac1n02x5",
    "i0soai012ad1n02x5"
  };

  AoiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvtRoot & "i0saoi012aa1n02x5.spf",
    NIL,
    StdCellLvtRoot & "i0saoi012ab1n02x5.spf",
    NIL,
    StdCellSvtRoot & "i0saoi012ac1n02x5.spf",
    StdCellHvtRoot & "i0saoi012ad1n02x5.spf"
  };

  OaiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvtRoot & "i0soai012aa1n02x5.spf",
    NIL,
    StdCellLvtRoot & "i0soai012ab1n02x5.spf",
    NIL,
    StdCellSvtRoot & "i0soai012ac1n02x5.spf",
    StdCellHvtRoot & "i0soai012ad1n02x5.spf"
  };

CONST
  Brand = "P1278p3TechProcess";

END P1278p3TechProcess.
                                


      
