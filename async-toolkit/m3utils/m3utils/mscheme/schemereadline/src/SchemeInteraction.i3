(* $Id$ *)

INTERFACE SchemeInteraction;
IMPORT SchemePrimitive;

PROCEDURE Extend(prims : SchemePrimitive.ExtDefiner)  : SchemePrimitive.ExtDefiner;

CONST Brand = "SchemeInteraction";

END SchemeInteraction.
