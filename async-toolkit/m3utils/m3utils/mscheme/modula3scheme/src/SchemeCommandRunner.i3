(* $Id$ *)

INTERFACE SchemeCommandRunner;
IMPORT SchemePrimitive;

CONST Brand = "SchemeCommandRunner";

PROCEDURE Extend(definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner;

END SchemeCommandRunner.
