(* $Id: SchemeInteraction.i3,v 1.3 2009/11/28 12:11:34 mika Exp $ *)

INTERFACE SchemeInteraction;
IMPORT Scheme;

CONST Brand = "SchemeInteraction";

PROCEDURE Hook(env : REFANY; do : Scheme.Object) : Scheme.Object RAISES { Scheme.E };

END SchemeInteraction.
