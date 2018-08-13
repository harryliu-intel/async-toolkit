INTERFACE MbyMappedKey;
IMPORT Byte;
FROM NetTypes IMPORT U16;

(* check types carefully, and, maybe we should have hierarchy here! *)

TYPE
  T = RECORD
    mapOuterProt : [0..16_f];
    mapOuterEtype : [0..16_f];
    mapInnerProt : [0..16_f];
    mapInnerEtype : [0..16_f];
    mapOuterDmac : Byte.T;
    mapOuterSmac : Byte.T;
    mapInnerDmac : Byte.T;
    mapInnerSmac : Byte.T;
    mapOuterDip : [0..16_f];
    mapOuterSip : [0..16_f];
    mapInnerDip : [0..16_f];
    mapInnerSip : [0..16_f];
    mapPort : Byte.T;
    mapOuterL4Dst : U16;
    mapOuterL4Src : U16;
    mapInnerL4Dst : U16;
    mapInnerL4Src : U16;
  END;

CONST Brand = "MbyMappedKey";

END MbyMappedKey.
