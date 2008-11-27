(* $Id$ *)

INTERFACE SXTime;
IMPORT Time, SXLongReal, SXInt;

CONST CurrentOffset = FIRST(LONGREAL);

PROCEDURE New(interval : Time.T; offset := 0.0d0) : SXLongReal.T;
(* start a thread that, every interval seconds, updates the output
   with the current time;

   pass in CurrentOffset to use the offset of the first call.
*)

PROCEDURE NewCounter(interval : Time.T; offset := 0.0d0) : SXInt.T;
(* as above, but counts in integers *)

PROCEDURE Next(interval, offset : Time.T) : Time.T;
  (* what is the next time (from now) at a given interval and offset
     (since the epoch) *)

END SXTime.
