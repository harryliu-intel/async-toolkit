INTERFACE RdlProperty;
IMPORT RdlPredefProperty;

TYPE
  T = ROOT BRANDED OBJECT END;

  Predef = T BRANDED OBJECT
    x : RdlPredefProperty.T;
  END;

  Userdef = T BRANDED OBJECT
    nm : TEXT;
  END;

CONST Brand = "RdlProperty";

END RdlProperty.
  
