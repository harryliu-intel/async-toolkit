INTERFACE BraceParse;
IMPORT Rd, Thread;
IMPORT AtomCellTbl;

PROCEDURE Parse(rd : Rd.T) : AtomCellTbl.T
  RAISES { Rd.Failure, Thread.Alerted };
  
END BraceParse.
