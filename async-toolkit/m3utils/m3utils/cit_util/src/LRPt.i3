(* $Id$
   Like LRPoint.i3, but field names are consistent with Point.i3.
*)

INTERFACE LRPt;

TYPE T = RECORD h, v : LONGREAL END;

CONST Brand = "LRPt";

PROCEDURE Normalize(p: T; newLength: LONGREAL): T;

END LRPt.
