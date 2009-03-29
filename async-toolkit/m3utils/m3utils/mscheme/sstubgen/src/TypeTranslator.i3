(* $Id$ *)

INTERFACE TypeTranslator;
IMPORT Type, SchemeObject;

PROCEDURE Translate(type : Type.T) : SchemeObject.T;

CONST Brand = "TypeTranslator";

END TypeTranslator.
