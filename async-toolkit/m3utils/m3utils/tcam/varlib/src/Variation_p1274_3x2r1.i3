INTERFACE Variation_p1274_3x2r1;
IMPORT Variation;

TYPE
  TranType = { psrhpc, p, nsrhdc, psrhdc, nsrlvc, navt, psrlvc, pavt, nsvt, psvt, n, nsrhpc };

CONST TranTypeNames = ARRAY TranType OF TEXT {
  "psrhpc", "p", "nsrhdc", "psrhdc", "nsrlvc", "navt", "psrlvc", "pavt", "nsvt", "psvt", "n", "nsrhpc" };

PROCEDURE ParseType(nm : TEXT) : TranType;

PROCEDURE New() : Variation.T;

(*CONST HspName ="/p/hdk/cad/process/p1274.3_sim/p1274.3x2r1_16ww37.4/hsp/p1274_3x2r1_var.hsp";*)
CONST HspName ="/p/hdk/cad/process/p1274.3_sim/p1274.3x2r1_16ww40.4/hsp/p1274_3x2r1_var.hsp";

(* the following are variables user must define -- geometry *)
(* in source we use get_o(scale) but here we just ask for microns directly *)
(* might be better to use meters? *)
CONST Geo = ARRAY OF TEXT { "M", "WdrawnUm", "LdrawnUm", "NF" };

(* the following are variables user must define -- variations *)
CONST Var = ARRAY OF TEXT { "lermat", "vtsingle" };
CONST VarParamsPerFet = NUMBER(Var);

CONST ConstParams = ARRAY OF Variation.Param {
  Variation.Param { "widtnflag", 1.0d0 },
  Variation.Param { "scale"    , 1.0d0 } };
      
END Variation_p1274_3x2r1.
