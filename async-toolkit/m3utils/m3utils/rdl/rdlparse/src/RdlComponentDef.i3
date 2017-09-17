INTERFACE RdlComponentDef;
IMPORT RdlRootElem;

(* must be opaque because of circular reference *)

TYPE T <: RdlRootElem.T;

CONST Brand = "RdlComponentDef";
      Equal : PROCEDURE (a, b : T): BOOLEAN = NIL;
      
END RdlComponentDef.
