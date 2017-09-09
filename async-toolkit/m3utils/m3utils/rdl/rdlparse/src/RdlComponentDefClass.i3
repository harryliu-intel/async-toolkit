INTERFACE RdlComponentDefClass;
IMPORT RdlComponentDef;
IMPORT RdlRootElem;
IMPORT RdlComponentDefType;

REVEAL
  RdlComponentDef.T = RdlRootElem.T BRANDED RdlComponentDef.Brand OBJECT
    type : RdlComponentDefType.T;
  END;

END RdlComponentDefClass.
