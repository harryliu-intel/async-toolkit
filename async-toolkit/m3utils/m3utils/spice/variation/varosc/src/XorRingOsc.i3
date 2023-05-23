INTERFACE XorRingOsc;
FROM TechConfig IMPORT Tran;
IMPORT Pathname;

CONST
  NP = 4;           (* number of parameters per transistor *)
  NV  = NT * NP;    (* variations per gate = 80 *)

  XaVar     = ARRAY [0..NP-1] OF TEXT { "gh:ghmat", "gh:lermat", "noia:noiv", "fms:vtsingle" };
  HspiceVar = ARRAY [0..NP-1] OF TEXT { "ghmat", "lermat", "noiv", "vtsingle" };

  NT = 10;

  XaTranSpec = ARRAY [ 0..NT-1] OF TEXT {
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

  HspiceTranSpec = ARRAY [ 0..NT-1] OF TEXT {
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

  PdkPath = "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk050_r3v2p0_efv";
  
  Z1Libs = ARRAY Tran OF Pathname.T {
  NIL,
  PdkPath & "/ldrdsibase_ulvt/spf/lib783_i0s_160h_50pp_ldrdsibase_ulvt_100c_tttt_ctyp",
  NIL,
  PdkPath & "/ldrdsibase_lvt/spf/lib783_i0s_160h_50pp_ldrdsibase_lvt_100c_tttt_ctyp",
  NIL,
  NIL,
  NIL
  };
  
  Z2Libs = ARRAY Tran OF Pathname.T {
  NIL,
  PdkPath & "/dsibase_ulvt/spf/lib783_i0s_160h_50pp_dsibase_ulvt_100c_tttt_ctyp",
  NIL,
  PdkPath & "/dsibase_lvt/spf/lib783_i0s_160h_50pp_dsibase_lvt_100c_tttt_ctyp",
  NIL,
  NIL,
  NIL
  };
  
  CellName = ARRAY [ 1 .. 2 ] OF ARRAY Tran OF TEXT {
    Z1Cells,
    Z2Cells
  };

  Z1Cells = ARRAY Tran OF TEXT {
  NIL,
  "i0sxor002aa1n01x1",
  NIL,
  "i0sxor002ab1n01x1",
  NIL,
  NIL,
  NIL
  };
  
  Z2Cells = ARRAY Tran OF TEXT {
  NIL,
  "i0sxor002aa1n02x5",
  NIL,
  "i0sxor002ab1n02x5",
  NIL,
  NIL,
  NIL
  };
  
END XorRingOsc.
