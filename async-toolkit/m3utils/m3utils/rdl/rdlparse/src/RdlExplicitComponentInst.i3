INTERFACE RdlExplicitComponentInst;
IMPORT RdlOrigin, RdlComponentInstElemList;
IMPORT RdlRootElem;

TYPE
  T = RdlRootElem.T OBJECT
    haveOrigin : BOOLEAN;
    origin     : RdlOrigin.T;
    alias      : TEXT;
    id         : TEXT;
    list       : RdlComponentInstElemList.T;
  END;

CONST Brand = "RdlExplicitComponentInst";

END RdlExplicitComponentInst.
