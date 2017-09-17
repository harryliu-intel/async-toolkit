INTERFACE RdlEnumDef;
IMPORT RdlRootElem;
IMPORT RdlEnumEntryList;

TYPE
  T = RdlRootElem.T BRANDED Brand OBJECT
    id : TEXT;
    body : RdlEnumEntryList.T;
  END;

CONST Brand = "RdlEnumDef";

END RdlEnumDef.
