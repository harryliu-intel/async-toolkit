INTERFACE CompMemory;
IMPORT CompRange, CsrAccessStatus, CsrOp;
IMPORT CompMemoryListener;

TYPE
  T <: Public;
  
  Public = OBJECT METHODS
    init(range : CompRange.T) : T;
    
    csrOp(VAR op : CsrOp.T) : CsrAccessStatus.T;

    registerListener(range : CompRange.T; listener : CompMemoryListener.T);
  END;

CONST Brand = "CompMemory";

VAR csrDebug := FALSE; (* set this to TRUE to turn on very verbose dbg *)
  
END CompMemory.
