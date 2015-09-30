(* $Id: SchemeAtRun.i3,v 1.1 2009/03/03 20:39:26 mika Exp $ *)

INTERFACE SchemeAtRun;
IMPORT SchemePrimitive;

CONST Brand = "SchemeAtRun";

PROCEDURE Extend(definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner;

END SchemeAtRun.
