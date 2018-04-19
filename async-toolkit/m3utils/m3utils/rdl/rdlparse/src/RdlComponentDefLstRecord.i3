INTERFACE RdlComponentDefLstRecord;
IMPORT RdlComponentDefElemList;
IMPORT RdlPropertySymtab;
IMPORT RdlComponentDefSymtab;

TYPE
  T = RECORD
    lst     : RdlComponentDefElemList.T;
    propTab : RdlPropertySymtab.T;
    defTab  : RdlComponentDefSymtab.T;
    anonCnt : CARDINAL;
  END;

CONST Brand = "RdlComponentDefLstRecord";

END RdlComponentDefLstRecord.
