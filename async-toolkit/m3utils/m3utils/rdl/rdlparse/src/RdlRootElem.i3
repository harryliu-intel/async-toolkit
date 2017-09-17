INTERFACE RdlRootElem;

TYPE
  T = BRANDED OBJECT END;

CONST Brand = "RdlRootElem";

      Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;
      
END RdlRootElem.
