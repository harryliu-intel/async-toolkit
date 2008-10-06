(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeLongReal;
IMPORT SchemeObject;
FROM Scheme IMPORT E;

TYPE T = REF LONGREAL;

PROCEDURE FromLR(x : LONGREAL) : T;

PROCEDURE FromO(x : SchemeObject.T) : LONGREAL RAISES { E };

CONST Brand = "SchemeLongReal";

PROCEDURE FromT(t : TEXT) : T RAISES { E }; (* my add'n *)

VAR (* CONST *) Zero, One : T;

END SchemeLongReal.
