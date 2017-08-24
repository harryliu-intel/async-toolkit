INTERFACE SimSupport;
IMPORT Src, BitInteger, Rdr;
FROM StandardSettings IMPORT Constraints;
IMPORT TextSeq;

TYPE
  IntegerSrc <: PubIntegerSrc;

  PubIntegerSrc = Src.T OBJECT 
    val : BitInteger.T;
  END;
    
  ClockedSrcIntf <: PubClockedSrcIntf;

  PubClockedSrcIntf = Src.T OBJECT
    ctr : REF Constraints := NIL;
    c   : ClockSrc;
  END;
  
  RMutexSrc  <: PubRMutexSrc;

  PubRMutexSrc = ClockedSrcIntf  OBJECT 
    s : Semaphore; 
    d : CARDINAL; 
    mutexVal := TRUE
  END;
  
  ArbSrc  <: ClockedSrcIntf;

  SeqSrc  <: PubSeqSrc;

  PubSeqSrc = ClockedSrcIntf OBJECT 
    q : REF ARRAY OF BitInteger.T;
  END;

  ClockedRdrIntf <: PubClockedRdrIntf;

  PubClockedRdrIntf = Rdr.T OBJECT
    ctr : Constraints;
    c   : ClockSrc;
  END;

  ClockSrc   <: PubClockSrc;

  PubClockSrc = Src.T OBJECT 
    lo, hi : LONGREAL;
    spd    : LONGREAL; 
  END;

  Reader     = ClockedRdrIntf BRANDED OBJECT END;

  Semaphore  = OBJECT
    ring : SemSrc := NIL;
  END;

  SemSrc = OBJECT
    src : Src.T;
    nxt : SemSrc;
  END;

PROCEDURE StrSeq(str : TEXT) : TextSeq.T; (* parse whitespace-separated list into tokens *)

END SimSupport.
