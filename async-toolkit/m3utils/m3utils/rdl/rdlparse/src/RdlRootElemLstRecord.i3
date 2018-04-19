INTERFACE RdlRootElemLstRecord;
IMPORT RdlRootElemList;
IMPORT RdlPropertySymtab;
IMPORT RdlComponentDefSymtab;

TYPE
  T = RECORD
    lst     : RdlRootElemList.T;
    propTab : RdlPropertySymtab.T;
    defTab  : RdlComponentDefSymtab.T;
  END;

CONST Brand = "RdlRootElemLstRecord";

END RdlRootElemLstRecord.
