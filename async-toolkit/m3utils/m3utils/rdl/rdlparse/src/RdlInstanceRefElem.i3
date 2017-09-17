INTERFACE RdlInstanceRefElem;
IMPORT RdlNum;

TYPE
  T = ROOT BRANDED OBJECT END;

  Id = T BRANDED OBJECT id : TEXT END;

  Brack = T BRANDED OBJECT id : TEXT; idx : RdlNum.T END;

CONST Brand = "RdlInstanceRefElem";

      Equal : PROCEDURE (a, b : T) : BOOLEAN = NIL;

END RdlInstanceRefElem.
