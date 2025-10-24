INTERFACE RdlPropertyRvalueConstant;
IMPORT RdlNum, RdlPropertyRvalueKeyword;

TYPE
  T = ROOT BRANDED OBJECT END;

  Num = T BRANDED OBJECT
    num : RdlNum.T;
  END;

  Str = T BRANDED OBJECT
    str : TEXT;
  END;

  Keyword = T BRANDED OBJECT
    kw : RdlPropertyRvalueKeyword.T;
  END;

CONST Brand = "RdlPropertyRvalueConstant";

END RdlPropertyRvalueConstant.
