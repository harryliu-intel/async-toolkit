(* $Id: SchemeLongReal.i3,v 1.8 2009/05/02 11:18:04 mika Exp $ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeLongReal;
IMPORT SchemeObject;
FROM Scheme IMPORT E;

TYPE T = REF LONGREAL;

PROCEDURE FromLR(x : LONGREAL) : T;

PROCEDURE FromI(x : INTEGER) : T;

PROCEDURE FromO(x : SchemeObject.T) : LONGREAL RAISES { E };

PROCEDURE Int(x : SchemeObject.T; roundOK := FALSE) : INTEGER RAISES { E };
  (* checks that operand is an integer if roundOK is FALSE *)

PROCEDURE Card(x : SchemeObject.T; roundOK := FALSE) : CARDINAL RAISES { E };
  (* checks that operand is an integer if roundOK is FALSE *)

CONST Brand = "SchemeLongReal";

PROCEDURE FromT(t : TEXT) : T RAISES { E }; (* my add'n *)

VAR (* CONST *) Zero, One : T;

END SchemeLongReal.
