(* $Id$ *)

INTERFACE BoolRemap;
IMPORT BoolBoolTbl;
IMPORT Bool;

TYPE Map = BoolBoolTbl.T;

(* for each variable pair b0, b1 in the map, change the occurrence of
   b0 to that of b1.  If check is TRUE, then abort if there are variables
   (except True and False) that have no mapping in the map. *)
PROCEDURE Remap(map : Map; e : Bool.T; check := FALSE) : Bool.T;

(* create new T and merge m1 and m2 into it *)
PROCEDURE Merge(m1, m2 : Map) : Map;

(* merge m1 and m2 into m1 and return m1 *)
PROCEDURE MergeD(m1, m2 : Map) : Map;

(* the empty map *)
PROCEDURE Empty() : Map;

END BoolRemap.
