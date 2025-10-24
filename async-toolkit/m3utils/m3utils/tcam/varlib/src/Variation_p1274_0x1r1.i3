INTERFACE Variation_p1274_0x1r1;
IMPORT Variation;

TYPE
  TranType = { psrhpc, p, nsrhdc, psrhdc, nsrlvc, navt, psrlvc, pavt, nsvt, psvt, n, nsrhpc };

CONST TranTypeNames = ARRAY TranType OF TEXT {
  "psrhpc", "p", "nsrhdc", "psrhdc", "nsrlvc", "navt", "psrlvc", "pavt", "nsvt", "psvt", "n", "nsrhpc" };

PROCEDURE ParseType(nm : TEXT) : TranType;

PROCEDURE New() : Variation.T;

CONST HspName ="/p/hdk/cad/process/p1274.2_sim/p1274.2_15ww17.1p1/hsp/p1274_0x1r1.hsp";

(* the following are variables user must define -- geometry *)
(* in source we use get_o(scale) but here we just ask for microns directly *)
(* might be better to use meters? *)
CONST Geo = ARRAY OF TEXT { "M", "WdrawnUm", "LdrawnUm", "NF" };

(* the following are variables user must define -- variations *)
CONST Var = ARRAY OF TEXT { "lermat", "vtsingle" };
CONST VarParamsPerFet = NUMBER(Var);
  
END Variation_p1274_0x1r1.
