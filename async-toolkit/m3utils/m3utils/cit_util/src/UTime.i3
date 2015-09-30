(* $Id: UTime.i3,v 1.1 2008/12/20 10:31:14 mika Exp $ *)

INTERFACE UTime;
IMPORT Time;

(* wrappers for Utime C stuff *)

PROCEDURE ctime(clock : Time.T;
                keepNL := TRUE;
                showTZ := FALSE) : TEXT;
  (* like the normal Unix ctime, but with some options... *)

CONST Brand = "UTime";

END UTime.
