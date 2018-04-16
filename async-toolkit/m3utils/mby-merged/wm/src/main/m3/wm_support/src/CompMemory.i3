INTERFACE CompMemory;
IMPORT CompRange, CsrAccessStatus, CsrOp;
IMPORT CompMemoryListener;

TYPE
  T <: Public;
  
  Public = OBJECT METHODS
    init(range : CompRange.T) : T;
    
    csrOp(op : CsrOp.T) : CsrAccessStatus.T;

    registerListener(range : CompRange.T; listener : CompMemoryListener.T);
  END;

CONST Brand = "CompMemory";

END CompMemory.
