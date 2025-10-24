INTERFACE MbyMapProfKey1;
IMPORT Byte;
FROM NetTypes IMPORT U16; (* U8? *)

(* all field types need to be checked carefully *)

TYPE
  T = RECORD
    ptype      : U16;
    l2Domain   : Byte.T;
    l3Domain   : Byte.T;
    ipScenario   : Byte.T;
    portProfile   : Byte.T;
    domainProfile   : Byte.T;
    macRoutable   : Byte.T;
    macMbcast   : Byte.T;
  END;

CONST Brand = "MbyMapProfKey1";

END MbyMapProfKey1.
