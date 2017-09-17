INTERFACE RdlAnonComponentInstElems;
IMPORT RdlComponentInstElemList;

TYPE
  T = OBJECT
    external : BOOLEAN;
    list : RdlComponentInstElemList.T;
  END;

CONST Brand = "RdlComponentInstElems";

END RdlAnonComponentInstElems.
