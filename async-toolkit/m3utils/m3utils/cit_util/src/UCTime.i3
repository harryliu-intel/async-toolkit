(* $Id: UCTime.i3,v 1.1 2008/12/24 10:52:36 mika Exp $ *)

INTERFACE UCTime;
IMPORT Time;

(* wrappers for Utime C stuff *)

PROCEDURE ctime(clock : Time.T;
                keepNL := TRUE;
                showTZ := FALSE) : TEXT;
  (* like the normal Unix ctime, but with some options... *)

CONST Brand = "UTime";

END UCTime.
