INTERFACE RegFieldAccess;
(* RDL Spec section 7.4 *)

TYPE
  Master = { SW, HW };

  Op     = { R, W };

  OpSet = SET OF Op;

  T = ARRAY Master OF OpSet;

CONST
  Brand = "RegFieldAccess";

CONST
  NA = SET OF Op {            };
  R  = SET OF Op { Op.R       };
  W  = SET OF Op {       Op.W };
  RW = SET OF Op { Op.R, Op.W };

END RegFieldAccess.
