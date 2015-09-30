(* $Id: AL.i3,v 1.1 2007/06/21 01:16:32 mika Exp $ *)

INTERFACE AL;
IMPORT AtomList;

(* utility interface for error messages *)

TYPE T = AtomList.T;

PROCEDURE Format(t : T) : TEXT;

END AL.
