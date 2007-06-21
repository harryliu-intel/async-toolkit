(* $Id$ *)

INTERFACE AL;
IMPORT AtomList;

(* utility interface for error messages *)

TYPE T = AtomList.T;

PROCEDURE Format(t : T) : TEXT;

END AL.
