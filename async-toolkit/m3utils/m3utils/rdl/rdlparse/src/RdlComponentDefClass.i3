INTERFACE RdlComponentDefClass;
IMPORT RdlComponentDef;
IMPORT RdlRootElem;
IMPORT RdlComponentDefType;
IMPORT RdlComponentDefLstRecord;
IMPORT RdlAnonComponentInstElems;

REVEAL
  RdlComponentDef.T = RdlRootElem.T BRANDED RdlComponentDef.Brand OBJECT
    type          : RdlComponentDefType.T;
    id            : TEXT;
    list          : RdlComponentDefLstRecord.T;
    anonInstElems : RdlAnonComponentInstElems.T;
  END;

END RdlComponentDefClass.
