(* $Id: LRPt.i3,v 1.2 2005/01/24 01:10:18 kp Exp $
   Like LRPoint.i3, but field names are consistent with Point.i3.
*)

INTERFACE LRPt;

TYPE T = RECORD h, v : LONGREAL END;

CONST Brand = "LRPt";

PROCEDURE Normalize(p: T; newLength: LONGREAL): T;
PROCEDURE Add(a,b: T): T;
PROCEDURE Sub(a,b: T): T;

END LRPt.
