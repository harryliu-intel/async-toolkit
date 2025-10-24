INTERFACE XorRingOsc;
FROM TechConfig IMPORT Tran;
IMPORT Pathname;
IMPORT P1278p3TechProcess AS Tech; (* yes, imported twice *)
IMPORT P1278p3TechProcess AS TechS;
IMPORT P1278p3_i0m_TechProcess AS TechM;


TYPE
  Z = [1 .. 3];
  
CONST
  NP = 4;           (* number of parameters per transistor *)
  NV = NT * NP;     (* variations per double gate = 2 * NV = 80 *)

  XaVar     = ARRAY [0..NP-1] OF TEXT { "gh:ghmat", "gh:lermat", "noia:noiv", "fms:vtsingle" };
  HspiceVar = ARRAY [0..NP-1] OF TEXT { "ghmat", "lermat", "noiv", "vtsingle" };

  NT = 10;

  XaTranSpec = ARRAY [ 0..NT-1 ] OF TEXT {
  "MMg1.qna.mn%Z%:@:nhp%TRANFLAVOR%%THRESH%.%Z%",
  "MMg1.qpa.mp%Z%:@:php%TRANFLAVOR%%THRESH%.%Z%",
  "MMg2.qns.mn%Z%:@:nhp%TRANFLAVOR%%THRESH%.%Z%",
  "MMg2.qpsb.mp%Z%:@:php%TRANFLAVOR%%THRESH%.%Z%",
  "MMg3.qna.mn%Z%:@:nhp%TRANFLAVOR%%THRESH%.%Z%",
  "MMg3.qpa.mp%Z%:@:php%TRANFLAVOR%%THRESH%.%Z%",
  "MMg4.qns.mn%Z%:@:nhp%TRANFLAVOR%%THRESH%.%Z%",
  "MMg4.qpsb.mp%Z%:@:php%TRANFLAVOR%%THRESH%.%Z%",
  "MMg5.qna.mn%Z%:@:nhp%TRANFLAVOR%%THRESH%.%Z%",
  "MMg5.qpa.mp%Z%:@:php%TRANFLAVOR%%THRESH%.%Z%"
};

  HspiceTranSpec = ARRAY [ 0..NT-1 ] OF TEXT {
  "MMg1.qna.mn%Z%:@:nhp%TRANFLAVOR%%THRESH%",
  "MMg1.qpa.mp%Z%:@:php%TRANFLAVOR%%THRESH%",
  "MMg2.qns.mn%Z%:@:nhp%TRANFLAVOR%%THRESH%",
  "MMg2.qpsb.mp%Z%:@:php%TRANFLAVOR%%THRESH%",
  "MMg3.qna.mn%Z%:@:nhp%TRANFLAVOR%%THRESH%",
  "MMg3.qpa.mp%Z%:@:php%TRANFLAVOR%%THRESH%",
  "MMg4.qns.mn%Z%:@:nhp%TRANFLAVOR%%THRESH%",
  "MMg4.qpsb.mp%Z%:@:php%TRANFLAVOR%%THRESH%",
  "MMg5.qna.mn%Z%:@:nhp%TRANFLAVOR%%THRESH%",
  "MMg5.qpa.mp%Z%:@:php%TRANFLAVOR%%THRESH%"
  };

  LibDir = ARRAY Tech.Stdcells OF ARRAY Z OF ARRAY Tran OF Pathname.T {
    I0sLibs, I0mLibs
  };


  I0sLibs = ARRAY Z OF ARRAY Tran OF Pathname.T {
    SZ1Libs,
    SZ2Libs,
    SZ3Libs
  };

  SZ1Libs = ARRAY Tran OF Pathname.T {
  NIL,
  TechS.XorStdCellUlvt1Root,
  NIL,
  TechS.XorStdCellLvt1Root,
  NIL,
  NIL,
  NIL
  };
  
  SZ2Libs = ARRAY Tran OF Pathname.T {
  NIL,
  TechS.XorStdCellUlvt2Root,
  NIL,
  TechS.XorStdCellLvt2Root,
  NIL,
  NIL,
  NIL
  };
  
  SZ3Libs = ARRAY Tran OF Pathname.T {
  NIL,
  TechS.XorStdCellUlvt3Root,
  NIL,
  TechS.XorStdCellLvt3Root,
  NIL,
  NIL,
  NIL
  };

  NilArray = ARRAY Tran OF TEXT { NIL, .. };
  
  I0mLibs = ARRAY Z OF ARRAY Tran OF Pathname.T {
    NilArray,
    MZ2Libs,
    MZ3Libs
  };


  MZ2Libs = ARRAY Tran OF Pathname.T {
  NIL,
  TechM.XorStdCellUlvtRoot,
  NIL,
  TechM.XorStdCellLvtRoot,
  NIL,
  NIL,
  NIL
  };
  
  MZ3Libs = ARRAY Tran OF Pathname.T {
  NIL,
  TechM.XorStdCellUlvtRoot,
  NIL,
  TechM.XorStdCellLvtRoot,
  NIL,
  NIL,
  NIL
  };

  (**********************************************************************)

  CellName = ARRAY Tech.Stdcells OF ARRAY Z OF ARRAY Tran OF TEXT {
    SCellName,
    MCellName
  };

  SCellName = ARRAY Z OF ARRAY Tran OF TEXT {
    SZ1Cells,
    SZ2Cells,
    SZ3Cells
  };

  SZ1Cells = TechS.Xor1CellNames;
  
  SZ2Cells = TechS.Xor2CellNames;

  SZ3Cells = TechS.Xor3CellNames;
  
  MCellName = ARRAY Z OF ARRAY Tran OF TEXT {
    NilArray,
    MZ2Cells,
    MZ3Cells
  };

  MZ2Cells = TechM.Xor2CellNames;

  MZ3Cells = TechM.Xor3CellNames;
  
END XorRingOsc.
