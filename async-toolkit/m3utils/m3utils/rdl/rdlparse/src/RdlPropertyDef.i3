INTERFACE RdlPropertyDef;
IMPORT RdlPropertyBody;
IMPORT RdlRootElem;

TYPE
  T = RdlRootElem.T OBJECT
    id   : TEXT;
    body : RdlPropertyBody.T;
  END;

CONST Brand = "RdlPropertyDef";

END RdlPropertyDef.
