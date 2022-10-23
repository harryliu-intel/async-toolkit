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
    P1278p3XorPaths,
    P1278p3BufPaths,
    P1278p3AoiCellPaths,
    P1278p3OaiCellPaths
  },

  cellNames :=ARRAY Gate OF ARRAY Tran OF TEXT {
    P1278p3XorCellNames,
    P1278p3BufCellNames,
    P1278p3AoiCellNames,
    P1278p3OaiCellNames
  },

  plugText :=""

      };

CONST
  Brand = "";

    P1278p3StdCellRoot = "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk030_r2v0p0_uv2_pre/";

  P1278p3StdCellUlvtRoot = P1278p3StdCellRoot & "/base_ulvt/spf/lib783_i0s_160h_50pp_base_ulvt_tttt_100c_cmax/";
  P1278p3StdCellLvtRoot = P1278p3StdCellRoot & "/base_lvt/spf/lib783_i0s_160h_50pp_base_lvt_tttt_100c_cmax/";
  P1278p3StdCellSvtRoot = P1278p3StdCellRoot & "/base_svt/spf/lib783_i0s_160h_50pp_base_svt_tttt_100c_cmax/";
  P1278p3StdCellHvtRoot = P1278p3StdCellRoot & "/base_hvt/spf/lib783_i0s_160h_50pp_base_hvt_tttt_100c_cmax/";
  
  P1278p3BufPaths = ARRAY Tran OF TEXT {
    NIL,
    P1278p3StdCellUlvtRoot & "i0sbfn000aa1n02x5.spf",
    NIL,
    P1278p3StdCellLvtRoot & "i0sbfn000ab1n02x5.spf",
    NIL,
    P1278p3StdCellSvtRoot & "i0sbfn000ac1n02x5.spf",
    P1278p3StdCellHvtRoot & "i0sbfn000ad1n02x5.spf"
  };
  
  P1278p3BufCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0sbfn000aa1n02x5",
    NIL,
    "i0sbfn000ab1n02x5",
    NIL,
    "i0sbfn000ac1n02x5",
    "i0sbfn000ad1n02x5"
  };

  P1278p3XorPaths = ARRAY Tran OF TEXT {
    NIL,
    P1278p3StdCellUlvtRoot & "i0sxor002aa1n02x5.spf",
    NIL,
    P1278p3StdCellLvtRoot & "i0sxor002ab1n02x5.spf",
    NIL,
    P1278p3StdCellSvtRoot & "i0sxor002ac1n02x5.spf",
    P1278p3StdCellHvtRoot & "i0sxor002ad1n02x5.spf"
  };
  
  P1278p3XorCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0sxor002aa1n02x5",
    NIL,
    "i0sxor002ab1n02x5",
    NIL,
    "i0sxor002ac1n02x5",
    "i0sxor002ad1n02x5"
  };

  P1278p3AoiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0saoi012aa1n02x5",
    NIL,
    "i0saoi012ab1n02x5",
    NIL,
    "i0saoi012ac1n02x5",
    "i0saoi012ad1n02x5"
  };

  P1278p3OaiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0soai012aa1n02x5",
    NIL,
    "i0soai012ab1n02x5",
    NIL,
    "i0soai012ac1n02x5",
    "i0soai012ad1n02x5"
  };

  P1278p3AoiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    P1278p3StdCellUlvtRoot & "i0saoi012aa1n02x5.spf",
    NIL,
    P1278p3StdCellLvtRoot & "i0saoi012ab1n02x5.spf",
    NIL,
    P1278p3StdCellSvtRoot & "i0saoi012ac1n02x5.spf",
    P1278p3StdCellHvtRoot & "i0saoi012ad1n02x5.spf"
  };

  P1278p3OaiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    P1278p3StdCellUlvtRoot & "i0soai012aa1n02x5.spf",
    NIL,
    P1278p3StdCellLvtRoot & "i0soai012ab1n02x5.spf",
    NIL,
    P1278p3StdCellSvtRoot & "i0soai012ac1n02x5.spf",
    P1278p3StdCellHvtRoot & "i0soai012ad1n02x5.spf"
  };

END P1278p3TechProcess.
                                


      
