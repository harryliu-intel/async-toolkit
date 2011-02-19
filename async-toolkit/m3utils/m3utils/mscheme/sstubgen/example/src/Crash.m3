(* $Id$ *)

MODULE Crash;

PROCEDURE Me() =
  VAR ptr : REF INTEGER := NIL;
  BEGIN
    ptr^ := 0
  END Me;

BEGIN END Crash.
  
