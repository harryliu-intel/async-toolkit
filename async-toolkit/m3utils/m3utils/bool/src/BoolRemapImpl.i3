(* $Id: BoolRemapImpl.i3,v 1.1 2001/11/26 22:45:38 mika Exp $ *)

INTERFACE BoolRemapImpl;
IMPORT Bool, BoolBoolTbl;

PROCEDURE Remap(map : BoolBoolTbl.T; e : Bool.T; check := FALSE) : Bool.T;

END BoolRemapImpl.
