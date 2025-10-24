(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE P1278p3_i0m_TechProcess;

IMPORT TechProcess;
FROM TechConfig  IMPORT Tran, Corn, Gate;
FROM TechProcess IMPORT TranSufxs;
FROM P1278p3TechProcess IMPORT HSpiceModels, HSpiceModelRoots;


CONST
  P = TechProcess.T {
  tranSufxs :=TranSufxs { NIL,       (* elvt *)
                          "hpaulvt", (* ulvt *)
                          NIL,       (* ulvtll *)
                          "hpalvt",  (* lvt *)
                          NIL,       (* lvtll *)
                          "hpasvt",  (* svt *)
                          "hpahvt"   (* hvt = svtll *)
  }

      ,
  tranSize := "w=2 l=14e-9 m=1 nf=1", (* w=2 is dubious here *)

  hspiceModel := "p1278_3.hsp",

  hspiceModelRoot := HSpiceModelRoots[HSpiceModels.R0p5],

  cornNames :=ARRAY Corn OF TEXT {
  "tttt",
  "rcss",
  "rcff",
  "rxsf",
  "rxfs"
  },

  cellPaths :=ARRAY Gate OF ARRAY Tran OF TEXT {
    XorPaths,
    XorPaths,
    BufPaths,
    AoiCellPaths,
    OaiCellPaths,
    Xor2Paths,
    Xor2Paths,
    Xor3Paths,
    Xor3Paths,

    Aoi2Paths,
    Aoi2Paths,
    Aoi3Paths,
    Aoi3Paths,

    Oai2Paths,
    Oai2Paths,
    Oai3Paths,
    Oai3Paths,

    NilPaths,
    Xor2Paths,
    Xor3Paths,
    
    Xor6Paths,
    Xor9Paths,
    Xor12Paths,
    Xor18Paths
  },
  

  cellNames :=ARRAY Gate OF ARRAY Tran OF TEXT {
    XorCellNames,
    XorCellNames,
    BufCellNames,
    AoiCellNames,
    OaiCellNames,
    Xor2CellNames,
    Xor2CellNames,
    Xor3CellNames,
    Xor3CellNames,

    Aoi2CellNames,
    Aoi2CellNames,
    Aoi3CellNames,
    Aoi3CellNames,

    Oai2CellNames,
    Oai2CellNames,
    Oai3CellNames,
    Oai3CellNames,

    NilCellNames,
    Xor2CellNames,
    Xor3CellNames,

    Xor6CellNames,
    Xor9CellNames,
    Xor12CellNames,
    Xor18CellNames
},

  plugText :=""

      };

  StdCellRoot = "/p/hdk/cad/stdcells/lib783_i0m_180h_50pp/pdk080_r4v2p0_efv";

  StdCellUlvtRoot = StdCellRoot & "/base_ulvt/spf/lib783_i0m_180h_50pp_base_ulvt_100c_tttt_ctyp/";
  StdCellLvtRoot = StdCellRoot & "/base_lvt/spf/lib783_i0m_180h_50pp_base_lvt_100c_tttt_ctyp/";
  StdCellSvtRoot = StdCellRoot & "/base_svt/spf/lib783_i0m_180h_50pp_base_svt_100c_tttt_ctyp/";
  StdCellHvtRoot = StdCellRoot & "/base_hvt/spf/lib783_i0m_180h_50pp_base_hvt_100c_tttt_ctyp/";

  XorStdCellUlvtRoot = StdCellRoot & "/dsibase_ulvt/spf/lib783_i0m_180h_50pp_dsibase_ulvt_100c_tttt_ctyp/";
  XorStdCellLvtRoot = StdCellRoot & "/dsibase_lvt/spf/lib783_i0m_180h_50pp_dsibase_lvt_100c_tttt_ctyp/";
  XorStdCellSvtRoot = StdCellRoot & "/dsibase_svt/spf/lib783_i0m_180h_50pp_dsibase_svt_100c_tttt_ctyp/";
  XorStdCellHvtRoot = StdCellRoot & "/dsibase_hvt/spf/lib783_i0m_180h_50pp_dsibase_hvt_100c_tttt_ctyp/";

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

  BufPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvtRoot & "i0mbfn000aa1n02x5.spf",
    NIL,
    StdCellLvtRoot & "i0mbfn000ab1n02x5.spf",
    NIL,
    StdCellSvtRoot & "i0mbfn000ac1n02x5.spf",
    StdCellHvtRoot & "i0mbfn000ad1n02x5.spf"
  };
  
  BufCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0mbfn000aa1n02x5",
    NIL,
    "i0mbfn000ab1n02x5",
    NIL,
    "i0mbfn000ac1n02x5",
    "i0mbfn000ad1n02x5"
  };

  XorPaths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvtRoot & "i0mxor002aa1n02x5.spf",
    NIL,
    XorStdCellLvtRoot & "i0mxor002ab1n02x5.spf",
    NIL,
    XorStdCellSvtRoot & "i0mxor002ac1n02x5.spf",
    XorStdCellHvtRoot & "i0mxor002ad1n02x5.spf"
  };

  Xor3Paths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvtRoot & "i0mxor002aa1n03x5.spf",
    NIL,
    XorStdCellLvtRoot & "i0mxor002ab1n03x5.spf",
    NIL,
    NIL,
    NIL
  };

  Xor6Paths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvtRoot & "i0mxor002aa1n06x5.spf",
    NIL,
    XorStdCellLvtRoot & "i0mxor002ab1n06x5.spf",
    NIL,
    NIL,
    NIL
  };
  Xor9Paths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvtRoot & "i0mxor002aa1d09x5.spf",
    NIL,
    XorStdCellLvtRoot & "i0mxor002ab1d09x5.spf",
    NIL,
    NIL,
    NIL
  };
  Xor12Paths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvtRoot & "i0mxor002aa1d12x5.spf",
    NIL,
    XorStdCellLvtRoot & "i0mxor002ab1d12x5.spf",
    NIL,
    NIL,
    NIL
  };
  Xor18Paths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvtRoot & "i0mxor002aa1d18x5.spf",
    NIL,
    XorStdCellLvtRoot & "i0mxor002ab1d18x5.spf",
    NIL,
    NIL,
    NIL
  };

  Xor2Paths = ARRAY Tran OF TEXT {
    NIL,
    XorStdCellUlvtRoot & "i0mxor002aa1n02x5.spf",
    NIL,
    XorStdCellLvtRoot & "i0mxor002ab1n02x5.spf",
    NIL,
    NIL,
    NIL
  };
  
  XorCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0mxor002aa1n02x5",
    NIL,
    "i0mxor002ab1n02x5",
    NIL,
    "i0mxor002ac1n02x5",
    "i0mxor002ad1n02x5"
  };

  Xor3CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0mxor002aa1n03x5",
    NIL,
    "i0mxor002ab1n03x5",
    NIL,
    "i0mxor002ac1n03x5",
    "i0mxor002ad1n03x5"
  };

  Xor6CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0mxor002aa1n06x5",
    NIL,
    "i0mxor002ab1n06x5",
    NIL,
    "i0mxor002ac1n06x5",
    "i0mxor002ad1n06x5"
  };

  Xor9CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0mxor002aa1d09x5",
    NIL,
    "i0mxor002ab1d09x5",
    NIL,
    "i0mxor002ac1d09x5",
    "i0mxor002ad1n09x5"
  };

  Xor12CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0mxor002aa1d12x5",
    NIL,
    "i0mxor002ab1d12x5",
    NIL,
    "i0mxor002ac1d12x5",
    "i0mxor002ad1d12x5"
  };

  Xor18CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0mxor002aa1d18x5",
    NIL,
    "i0mxor002ab1d18x5",
    NIL,
    "i0mxor002ac1d18x5",
    "i0mxor002ad1d18x5"
  };

  Xor2CellNames = XorCellNames;

  AoiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0maoi012aa1n02x5",
    NIL,
    "i0maoi012ab1n02x5",
    NIL,
    "i0maoi012ac1n02x5",
    "i0maoi012ad1n02x5"
  };

  Aoi3CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0maoi012aa1n03x5",
    NIL,
    "i0maoi012ab1n03x5",
    NIL,
    "i0maoi012ac1n03x5",
    "i0maoi012ad1n03x5"
  };

  Aoi2CellNames = AoiCellNames;

  OaiCellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0moai012aa1n02x5",
    NIL,
    "i0moai012ab1n02x5",
    NIL,
    "i0moai012ac1n02x5",
    "i0moai012ad1n02x5"
  };

  Oai3CellNames = ARRAY Tran OF TEXT {
    NIL,
    "i0moai012aa1n03x5",
    NIL,
    "i0moai012ab1n03x5",
    NIL,
    "i0moai012ac1n03x5",
    "i0moai012ad1n03x5"
  };

  Oai2CellNames = OaiCellNames;

  AoiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvtRoot & "i0maoi012aa1n02x5.spf",
    NIL,
    StdCellLvtRoot & "i0maoi012ab1n02x5.spf",
    NIL,
    StdCellSvtRoot & "i0maoi012ac1n02x5.spf",
    StdCellHvtRoot & "i0maoi012ad1n02x5.spf"
  };

  Aoi3Paths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvtRoot & "i0maoi012aa1n03x5.spf",
    NIL,
    StdCellLvtRoot & "i0maoi012ab1n03x5.spf",
    NIL,
    StdCellSvtRoot & "i0maoi012ac1n03x5.spf",
    StdCellHvtRoot & "i0maoi012ad1n03x5.spf"
  };

  Aoi2Paths = AoiCellPaths;

  OaiCellPaths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvtRoot & "i0moai012aa1n02x5.spf",
    NIL,
    StdCellLvtRoot & "i0moai012ab1n02x5.spf",
    NIL,
    StdCellSvtRoot & "i0moai012ac1n02x5.spf",
    StdCellHvtRoot & "i0moai012ad1n02x5.spf"
  };

  Oai3Paths = ARRAY Tran OF TEXT {
    NIL,
    StdCellUlvtRoot & "i0moai012aa1n03x5.spf",
    NIL,
    StdCellLvtRoot & "i0moai012ab1n03x5.spf",
    NIL,
    StdCellSvtRoot & "i0moai012ac1n03x5.spf",
    StdCellHvtRoot & "i0moai012ad1n03x5.spf"
  };

  Oai2Paths = OaiCellPaths;
  
CONST
  Brand = "P1278p3_i0m_TechProcess";

END P1278p3_i0m_TechProcess.
                                


      
