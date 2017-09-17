INTERFACE RdlExplicitComponentInst;
IMPORT RdlOrigin, RdlComponentInstElemList;

TYPE
  T = OBJECT
    haveOrigin : BOOLEAN;
    origin : RdlOrigin.T;
    alias : TEXT;
    id : TEXT;
    list : RdlComponentInstElemList.T;
  END;

CONST Brand = "RdlExplicitComponentInst";

END RdlExplicitComponentInst.
