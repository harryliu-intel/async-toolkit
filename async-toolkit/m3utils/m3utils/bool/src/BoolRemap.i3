(* $Id$ *)

INTERFACE BoolRemap;
IMPORT BoolBoolTbl;
FROM Bool IMPORT T;

(* for each variable pair b0, b1 in the map, change the occurrence of
   b0 to that of b1.  If check is TRUE, then abort if there are variables
   (except True and False) that have no mapping in the map. *)
PROCEDURE Remap(map : BoolBoolTbl.T; e : T; check := FALSE) : T;


END BoolRemap.
