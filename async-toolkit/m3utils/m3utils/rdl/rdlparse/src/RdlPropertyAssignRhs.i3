INTERFACE RdlPropertyAssignRhs;
IMPORT RdlPropertyRvalueConstant;
IMPORT RdlInstanceRef;
IMPORT RdlConcatElemList;
IMPORT RdlEnumEntryList;

TYPE
  T = ROOT BRANDED OBJECT END;

  Const = T BRANDED OBJECT
    const : RdlPropertyRvalueConstant.T;
  END;

  Enum = T BRANDED OBJECT
    enum : RdlEnumEntryList.T;
  END;

  Iref = T BRANDED OBJECT
    iref : RdlInstanceRef.T;
  END;

  Concat = T BRANDED OBJECT
    concat : RdlConcatElemList.T;
  END;

CONST Brand = "RdlPropertyAssignRhs";

PROCEDURE Format(t : T) : TEXT;
  
END RdlPropertyAssignRhs.
