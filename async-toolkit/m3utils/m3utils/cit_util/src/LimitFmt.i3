(* $Id: LimitFmt.i3,v 1.1 2009/02/12 00:14:09 mika Exp $ *)
INTERFACE LimitFmt;

PROCEDURE LongReal(lr, maxError : LONGREAL) : TEXT; 
(* format lr with the fewest digits s.t. its max (relative) error is maxError *)

END LimitFmt.


