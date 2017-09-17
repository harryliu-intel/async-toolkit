INTERFACE RdlConcatElem;
IMPORT RdlInstanceRef;
IMPORT RdlNum;

TYPE
  T = ROOT BRANDED OBJECT END;

  Iref = T BRANDED OBJECT
    iref : RdlInstanceRef.T;
  END;

  Num = T BRANDED OBJECT
    num : RdlNum.T;
  END;

CONST Brand = "RdlConcatElem";
      Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;
END RdlConcatElem.
