INTERFACE RdlArray;
IMPORT RdlNum;

TYPE
  T = ROOT BRANDED OBJECT END;

  Single = T BRANDED OBJECT n : RdlNum.T END;

  Range = T BRANDED OBJECT frm, to : RdlNum.T END;

CONST Brand = "RdlArray";

END RdlArray.
