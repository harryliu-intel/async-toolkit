INTERFACE LibertyComponentChildren;
IMPORT LibertyComponent, LibertyComponentSeq;

REVEAL
  LibertyComponent.T <: Private;

TYPE
  Private = LibertyComponent.Public OBJECT METHODS
    children() : LibertyComponentSeq.T;
  END;

CONST Brand = "LibertyComponentChildren";

END LibertyComponentChildren.
    
  
