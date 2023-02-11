INTERFACE P1276p4TechProcess;

IMPORT TechProcess;
FROM TechConfig  IMPORT Tran, Corn, Gate;
FROM TechProcess IMPORT TranSufxs;

CONST
  P = TechProcess.T {
  tranSufxs := TranSufxs { NIL,      (* elvt *)
                "hpculvt", (* ulvt *)
                NIL,      (* ulvtll *)
                "hplvt",  (* lvt *)
                NIL,      (* lvtll *)
                "hpsvt",  (* svt *)
                NIL       (* svtll *)
  }      ,
  tranSize :="w=480e-9 l=14e-9 m=1",

  hspiceModel :="p1276_4.hsp",


  hspiceModelRoot := "/p/hdk/cad/pdk/pdk764_r0.9.1_22ww53.3/models/core/hspice/m17_6x_2ya_2yb_2yc_2yd_1ye_1ga_mim3x_1gb__bumpp/",

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

  (*
  StdCellRoot = "/p/hdk/cad/stdcells/g1m/22ww37.5_p1276d4_g1m_b.0.p3.core/spf/p1276d4_tttt_v0550_t100_pdn_max/";
  *)

  StdCellRoot = "/p/hdk/cad/stdcells/lib764_g1i_210h_50pp/pdk090_r4v0p0_fv_ext/";

  UlvtSpfDir  = StdCellRoot & "base_ulvt/spf/lib764_g1i_210h_50pp_base_ulvt_tttt_105c_tttt_ctyp/";

  LvtSpfDir  = StdCellRoot & "base_lvt/spf/lib764_g1i_210h_50pp_base_lvt_tttt_105c_tttt_ctyp/";
  
  SvtSpfDir  = StdCellRoot & "base_svt/spf/lib764_g1i_210h_50pp_base_svt_tttt_105c_tttt_ctyp/";

  
  BufCellPaths = ARRAY Tran OF TEXT {
    NIL, 
    UlvtSpfDir & "g1ibfn000aa1n02x5.spf",
    NIL,
    LvtSpfDir  & "g1ibfn000ab1n02x5.spf",
    NIL,
    SvtSpfDir  & "g1ibfn000ac1n02x5.spf",
    NIL
  };

  BufCellNames = ARRAY Tran OF TEXT {
    NIL,
    "g1ibfn000aa1n02x5",
    NIL,
    "g1ibfn000ab1n02x5",
    NIL,
    "g1ibfn000ac1n02x5",
    NIL
  };

  XorCellPaths = ARRAY Tran OF TEXT {
    NIL,
    UlvtSpfDir & "g1ixorc02aa1n02x5.spf",
    NIL,
    LvtSpfDir  & "g1ixorc02ab1n02x5.spf",
    NIL,
    SvtSpfDir  & "g1ixorc02ac1n02x5.spf",
    NIL
  };

  XorCellNames = ARRAY Tran OF TEXT {
    (* why the heck is this 02x4? *)
    NIL,
    "g1ixorc02aa1n02x5",
    NIL,
    "g1ixorc02ab1n02x5",
    NIL,
    "g1ixorc02ac1n02x5",
    NIL
  };

  AoiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "g1iaoi012aa1n02x5",
    NIL,
    "g1iaoi012ab1n02x5",
    NIL,
    "g1iaoi012ac1n02x5",
    NIL
  };

  OaiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "g1ioai012aa1n02x5",
    NIL,
    "g1ioai012ab1n02x5",
    NIL,
    "g1ioai012ac1n02x5",
    NIL
  };

  
  AoiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    UlvtSpfDir  & "g1iaoi012aa1n02x5.spf",
    NIL,
    LvtSpfDir   & "g1iaoi012ab1n02x5.spf",
    NIL,
    SvtSpfDir   & "g1iaoi012ac1n02x5.spf",
    NIL
  };

  OaiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    UlvtSpfDir   & "g1ioai012aa1n02x5.spf",
    NIL,
    LvtSpfDir    & "g1ioai012ab1n02x5.spf",
    NIL,
    SvtSpfDir    & "g1ioai012ac1n02x5.spf",
    NIL
  };

CONST
  Brand = "P1276p4TechProcess";

END P1276p4TechProcess.
                                


      
