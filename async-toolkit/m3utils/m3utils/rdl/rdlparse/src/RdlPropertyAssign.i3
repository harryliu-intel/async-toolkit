INTERFACE RdlPropertyAssign;
IMPORT RdlRootElem;

TYPE T = RdlRootElem.T BRANDED OBJECT END;

CONST Brand = "RdlPropertyAssign";

      Equal : PROCEDURE (a, b : T) : BOOLEAN = NIL;
      
END RdlPropertyAssign.
