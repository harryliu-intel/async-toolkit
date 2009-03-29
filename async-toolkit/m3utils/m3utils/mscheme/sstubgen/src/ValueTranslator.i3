(* $Id$ *)

INTERFACE ValueTranslator;
IMPORT Value, SchemeObject;

PROCEDURE Translate(value : Value.T) : SchemeObject.T;

CONST Brand = "ValueTranslator";

END ValueTranslator.
