INTERFACE P1276p4_aml2_TechProcess;

(* Andrew's extracted layout *)

IMPORT TechProcess;
FROM TechConfig  IMPORT Tran, Corn, Gate;
FROM TechProcess IMPORT TranSufxs;

CONST
  P = TechProcess.T {
  tranSufxs := TranSufxs { NIL,      (* elvt *)
                "hpculvt", (* ulvt *)
                NIL,      (* ulvtll *)
                NIL,  (* lvt *)
                NIL,      (* lvtll *)
                NIL,  (* svt *)
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
    OaiCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths,

    NilCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths,

    NilCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths,
    NilCellPaths
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

    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,

    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames,
    NilCellNames
  },

  plugText := ""

      };

  AmlExtracted = "/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/g1i_aml/g1i_extracted.hspice";
  
  NilCellPaths = ARRAY Tran OF TEXT {
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL
  };
  NilCellNames = ARRAY Tran OF TEXT {
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL
  };

  BufCellPaths = ARRAY Tran OF TEXT {
    NIL, 
    AmlExtracted,
    NIL,
    NIL,
    NIL,
    NIL,
    NIL
  };

  BufCellNames = ARRAY Tran OF TEXT {
    NIL,
    "g1ibfn000aa2n02x5",
    NIL,
    NIL,
    NIL,
    NIL,
    NIL
  };

  XorCellPaths = ARRAY Tran OF TEXT {
    NIL,
    AmlExtracted,
    NIL,
    NIL,
    NIL,
    NIL,
    NIL
  };

  XorCellNames = ARRAY Tran OF TEXT {
    (* why the heck is this 02x4? *)
    NIL,
    "g1ixor002aa2n02x5",
    NIL,
    NIL,
    NIL,
    NIL,
    NIL
  };

  AoiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "g1iaoi012aa2n02x5",
    NIL,
    NIL,
    NIL,
    NIL,
    NIL
  };

  OaiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "g1ioai012aa2n02x5",
    NIL,
    NIL,
    NIL,
    NIL,
    NIL
  };

  
  AoiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    AmlExtracted,
    NIL,
    NIL,
    NIL,
    NIL,
    NIL
  };

  OaiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    AmlExtracted,
    NIL,
    NIL,
    NIL,
    NIL,
    NIL
  };

CONST
  Brand = "P1276p4_aml2_TechProcess";

END P1276p4_aml2_TechProcess.
                                


      
