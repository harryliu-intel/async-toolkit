INTERFACE StdfParseTable;
IMPORT StdfRecordTypes;

PROCEDURE Get(recTyp, recSub : CARDINAL; VAR res : StdfRecordTypes.T) : BOOLEAN;

CONST Brand = "StdfParseTable";

END StdfParseTable.
