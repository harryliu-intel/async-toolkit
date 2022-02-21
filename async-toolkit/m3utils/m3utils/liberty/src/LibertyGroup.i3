INTERFACE LibertyGroup;
IMPORT LibertyComponent;
IMPORT LibertyHead;
IMPORT LibertyStatementSeq;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    head : LibertyHead.T;
    statements : LibertyStatementSeq.T;
  END;

CONST Brand = "LibertyGroup";

END LibertyGroup.
 
