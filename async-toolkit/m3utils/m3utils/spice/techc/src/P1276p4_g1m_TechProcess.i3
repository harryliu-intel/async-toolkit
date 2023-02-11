INTERFACE P1276p4_g1m_TechProcess;

IMPORT TechProcess;
FROM TechConfig  IMPORT Tran, Corn, Gate;
FROM TechProcess IMPORT TranSufxs;

CONST
  P = TechProcess.T {
  tranSufxs := TranSufxs { NIL,      (* elvt *)
                "hpulvt", (* ulvt *)
                NIL,      (* ulvtll *)
                "hplvt",  (* lvt *)
                NIL,      (* lvtll *)
                "hpsvt",  (* svt *)
                NIL       (* svtll *)
  }      ,
  tranSize :="L=0.014u W=0.06u",

  hspiceModel :="p1276_4.hsp",


  hspiceModelRoot :="/p/hdk/cad/pdk/pdk764_r0.5_22ww20.5/models/core/hspice/m17_6x_2ya_2yb_2yc_2yd_1ye_1ga_mim3x_1gb__bumpp",

  cornNames :=ARRAY Corn OF TEXT {
  "tttt",
  "psss",
  "pfff",
  "rssf",
  "rsfs"
  },

  cellPaths :=ARRAY Gate OF ARRAY Tran OF TEXT {
    XorCellPaths,
    XorCellPaths,
    BufCellPaths,
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

  plugText := ""

      };

  StdCellRoot = "/p/hdk/cad/stdcells/g1m/22ww37.5_p1276d4_g1m_b.0.p3.core/spf/p1276d4_tttt_v0550_t100_pdn_max/";

  BufCellPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellRoot & "an/g1mbfn000aa1n02x5.spf",
    NIL,
    StdCellRoot & "bn/g1mbfn000ab1n02x5.spf",
    NIL,
    StdCellRoot & "cn/g1mbfn000ac1n02x5.spf",
    NIL
  };

  BufCellNames = ARRAY Tran OF TEXT {
    NIL,
    "g1mbfn000aa1n02x5",
    NIL,
    "g1mbfn000ab1n02x5",
    NIL,
    "g1mbfn000ac1n02x5",
    NIL
  };

  XorCellPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellRoot & "an/g1mxor002aa1n02x4.spf",
    NIL,
    StdCellRoot & "bn/g1mxor002ab1n02x4.spf",
    NIL,
    StdCellRoot & "cn/g1mxor002ac1n02x4.spf",
    NIL
  };

  XorCellNames = ARRAY Tran OF TEXT {
    (* why the heck is this 02x4? *)
    NIL,
    "g1mxor002aa1n02x4",
    NIL,
    "g1mxor002ab1n02x4",
    NIL,
    "g1mxor002ac1n02x4",
    NIL
  };

  AoiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "g1maoi012aa1n02x5",
    NIL,
    "g1maoi012ab1n02x5",
    NIL,
    "g1maoi012ac1n02x5",
    NIL
  };

  OaiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "g1moai012aa1n02x5",
    NIL,
    "g1moai012ab1n02x5",
    NIL,
    "g1moai012ac1n02x5",
    NIL
  };

  
  AoiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellRoot & "an/g1maoi012aa1n02x5.spf",
    NIL,
    StdCellRoot & "bn/g1maoi012ab1n02x5.spf",
    NIL,
    StdCellRoot & "cn/g1maoi012ac1n02x5.spf",
    NIL
  };

  OaiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellRoot & "an/g1moai012aa1n02x5.spf",
    NIL,
    StdCellRoot & "bn/g1moai012ab1n02x5.spf",
    NIL,
    StdCellRoot & "cn/g1moai012ac1n02x5.spf",
    NIL
  };

CONST
  Brand = "P1276p4_g1m_TechProcess";

END P1276p4_g1m_TechProcess.
                                


      
