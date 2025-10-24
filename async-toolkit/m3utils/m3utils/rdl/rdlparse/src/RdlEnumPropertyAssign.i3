INTERFACE RdlEnumPropertyAssign;

TYPE T = ROOT BRANDED OBJECT END;

     Name = T BRANDED OBJECT str : TEXT END;

     Desc = T BRANDED OBJECT str : TEXT END;

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;

      Brand = "RdlPropertyAssign";

END RdlEnumPropertyAssign.
