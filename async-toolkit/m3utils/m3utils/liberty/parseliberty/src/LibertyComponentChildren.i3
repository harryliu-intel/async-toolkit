INTERFACE LibertyComponentChildren;
IMPORT LibertyComponent, LibertyComponentSeq;

REVEAL
  LibertyComponent.T <: Private;

TYPE
  Private = LibertyComponent.Public OBJECT METHODS
    canHaveChildren() : BOOLEAN;
    children() : LibertyComponentSeq.T;
    debugDump(truncate : CARDINAL := LAST(CARDINAL)) : TEXT;
  END;

CONST Brand = "LibertyComponentChildren";

END LibertyComponentChildren.
    
  
