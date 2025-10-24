INTERFACE RdlPropertyType;
IMPORT RdlPropertyRefType;

TYPE
  T = ROOT BRANDED OBJECT END;

  String = T BRANDED OBJECT END;

  Number = T BRANDED OBJECT END;

  Boolean = T BRANDED OBJECT END;

  Ref = T BRANDED OBJECT
    to : RdlPropertyRefType.T;
  END;

CONST Brand = "RdlPropertyType";

END RdlPropertyType.
