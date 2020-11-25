INTERFACE BraceParse;
IMPORT Rd, Thread;
IMPORT AtomCellTbl;
IMPORT CharSeq;

TYPE
  T = OBJECT
    cellTbl : AtomCellTbl.T;
    longNames : CharSeq.T;
  END;
  
PROCEDURE Parse(rd : Rd.T) : T
  RAISES { Rd.Failure, Thread.Alerted };
  
END BraceParse.
