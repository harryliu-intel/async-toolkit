(* $Id$ *)

INTERFACE SchemeInteraction;
IMPORT Scheme;

CONST Brand = "SchemeInteraction";

PROCEDURE Hook(env : REFANY) : Scheme.Object RAISES { Scheme.E };

END SchemeInteraction.
