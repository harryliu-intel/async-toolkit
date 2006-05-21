(* $Id$ *)

INTERFACE LRArrayOps;

PROCEDURE Sort(VAR a : ARRAY OF LONGREAL);

PROCEDURE Percentile(READONLY a : ARRAY OF LONGREAL; p : LONGREAL) : LONGREAL;
  (* a must be sorted *)

END LRArrayOps.
