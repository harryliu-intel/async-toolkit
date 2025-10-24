INTERFACE RdlPropertyDefault;
IMPORT RdlStr, RdlNum;

TYPE
  T = ROOT BRANDED OBJECT END;

  Str = T OBJECT
    str : RdlStr.T;
  END;

  Num = T OBJECT
    num : RdlNum.T;
  END;

  Boolean = T OBJECT
    val : BOOLEAN;
  END;

CONST Brand = "RdlPropertyDefault";

END RdlPropertyDefault.
