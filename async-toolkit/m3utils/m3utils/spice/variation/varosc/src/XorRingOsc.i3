INTERFACE XorRingOsc;
FROM TechConfig IMPORT Tran;
IMPORT Pathname;
IMPORT P1278p3TechProcess AS Tech;

CONST
  NP = 4;           (* number of parameters per transistor *)
  NV = NT * NP;     (* variations per double gate = 2 * NV = 80 *)

  XaVar     = ARRAY [0..NP-1] OF TEXT { "gh:ghmat", "gh:lermat", "noia:noiv", "fms:vtsingle" };
  HspiceVar = ARRAY [0..NP-1] OF TEXT { "ghmat", "lermat", "noiv", "vtsingle" };

  NT = 10;

  XaTranSpec = ARRAY [ 0..NT-1 ] OF TEXT {
  "MMg1.qna.mn%Z%:@:nhpb%THRESH%.%Z%",
  "MMg1.qpa.mp%Z%:@:phpb%THRESH%.%Z%",
  "MMg2.qns.mn%Z%:@:nhpb%THRESH%.%Z%",
  "MMg2.qpsb.mp%Z%:@:phpb%THRESH%.%Z%",
  "MMg3.qna.mn%Z%:@:nhpb%THRESH%.%Z%",
  "MMg3.qpa.mp%Z%:@:phpb%THRESH%.%Z%",
  "MMg4.qns.mn%Z%:@:nhpb%THRESH%.%Z%",
  "MMg4.qpsb.mp%Z%:@:phpb%THRESH%.%Z%",
  "MMg5.qna.mn%Z%:@:nhpb%THRESH%.%Z%",
  "MMg5.qpa.mp%Z%:@:phpb%THRESH%.%Z%"
};

  HspiceTranSpec = ARRAY [ 0..NT-1 ] OF TEXT {
  "MMg1.qna.mn%Z%:@:nhpb%THRESH%",
  "MMg1.qpa.mp%Z%:@:phpb%THRESH%",
  "MMg2.qns.mn%Z%:@:nhpb%THRESH%",
  "MMg2.qpsb.mp%Z%:@:phpb%THRESH%",
  "MMg3.qna.mn%Z%:@:nhpb%THRESH%",
  "MMg3.qpa.mp%Z%:@:phpb%THRESH%",
  "MMg4.qns.mn%Z%:@:nhpb%THRESH%",
  "MMg4.qpsb.mp%Z%:@:phpb%THRESH%",
  "MMg5.qna.mn%Z%:@:nhpb%THRESH%",
  "MMg5.qpa.mp%Z%:@:phpb%THRESH%"
  };


  LibDir = ARRAY [ 1 .. 2 ] OF ARRAY Tran OF Pathname.T {
    Z1Libs,
    Z2Libs
  };

  Z1Libs = ARRAY Tran OF Pathname.T {
  NIL,
  Tech.XorStdCellUlvt1Root,
  NIL,
  Tech.XorStdCellLvt1Root,
  NIL,
  NIL,
  NIL
  };
  
  Z2Libs = ARRAY Tran OF Pathname.T {
  NIL,
  Tech.XorStdCellUlvt2Root,
  NIL,
  Tech.XorStdCellLvt2Root,
  NIL,
  NIL,
  NIL
  };
  
  CellName = ARRAY [ 1 .. 2 ] OF ARRAY Tran OF TEXT {
    Z1Cells,
    Z2Cells
  };

  Z1Cells = Tech.Xor1CellNames;
  
  Z2Cells = Tech.Xor2CellNames;
  
END XorRingOsc.
