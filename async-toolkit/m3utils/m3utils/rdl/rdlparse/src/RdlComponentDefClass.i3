INTERFACE RdlComponentDefClass;
IMPORT RdlComponentDef;
IMPORT RdlRootElem;
IMPORT RdlComponentDefType;
IMPORT RdlComponentDefElemList;
IMPORT RdlAnonComponentInstElems;

REVEAL
  RdlComponentDef.T = RdlRootElem.T BRANDED RdlComponentDef.Brand OBJECT
    type : RdlComponentDefType.T;
    id : TEXT;
    list : RdlComponentDefElemList.T;
    anonInstElems : RdlAnonComponentInstElems.T;
  END;

END RdlComponentDefClass.
