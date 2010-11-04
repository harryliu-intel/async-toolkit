(* $Id$ *)

GENERIC INTERFACE TblMap(Tbl, Map);

(* 
   A TblMap.T is a wrapper for a Tbl, an object that obeys the generic
   Table interface at least to the extent of its get method, and that
   allows the Table to be accessed as a Map 
*)

PROCEDURE Wrap(tbl : Tbl.T) : Map.T; 
  (* return a Map for which a nonexistent mapping is an error *)

PROCEDURE WrapWithDefault(tbl : Tbl.T; default : Map.Result) : Map.T; 
  (* return a Map for which a nonexistent mapping is an error *)

CONST Brand = "TblMap(" & Tbl.Brand & "," & Map.Brand & ")";

END TblMap.
