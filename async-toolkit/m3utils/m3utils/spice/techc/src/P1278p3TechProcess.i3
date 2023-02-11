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


  hspiceModelRoot :="/p/hdk/cad/pdk/pdk783_r0.3.1_22ww38.7/models/core/hspice/m16_2x_1xa_1xb_6ya_2yb_2yc_2yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp",

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
    OaiCellPaths
  },

  cellNames :=ARRAY Gate OF ARRAY Tran OF TEXT {
    XorCellNames,
    XorCellNames,
    BufCellNames,
    AoiCellNames,
    OaiCellNames
  },

  plugText :=""

      };

  StdCellRoot = "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk030_r2v0p0_uv2_pre/";

  StdCellUlvtRoot = StdCellRoot & "/base_ulvt/spf/lib783_i0s_160h_50pp_base_ulvt_tttt_100c_cmax/";
  StdCellLvtRoot = StdCellRoot & "/base_lvt/spf/lib783_i0s_160h_50pp_base_lvt_tttt_100c_cmax/";
  StdCellSvtRoot = StdCellRoot & "/base_svt/spf/lib783_i0s_160h_50pp_base_svt_tttt_100c_cmax/";
  StdCellHvtRoot = StdCellRoot & "/base_hvt/spf/lib783_i0s_160h_50pp_base_hvt_tttt_100c_cmax/";
  
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
  
  XorCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0sxor002aa1n02x5",
    NIL,
    "i0sxor002ab1n02x5",
    NIL,
    "i0sxor002ac1n02x5",
    "i0sxor002ad1n02x5"
  };

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
                                


      
