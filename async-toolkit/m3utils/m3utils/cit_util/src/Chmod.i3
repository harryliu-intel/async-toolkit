(* $Id: Chmod.i3,v 1.1 2006/03/12 11:05:32 mika Exp $ *)

INTERFACE Chmod;
IMPORT Pathname;

EXCEPTION Error;

PROCEDURE chmod(path : Pathname.T; mode : CARDINAL) RAISES { Error };

END Chmod.
