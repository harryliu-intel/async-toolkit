INTERFACE RdlEnumEntry;
IMPORT RdlNum;
IMPORT RdlEnumPropertyAssignList;

TYPE
  T = OBJECT
    id : TEXT;
    num : RdlNum.T;
    properties : RdlEnumPropertyAssignList.T;
  END;

CONST Brand = "RdlEnumEntry";

      Equal : PROCEDURE (a, b : T): BOOLEAN = NIL;

END RdlEnumEntry.
