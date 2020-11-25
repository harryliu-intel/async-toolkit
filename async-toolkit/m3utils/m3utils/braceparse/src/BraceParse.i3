INTERFACE BraceParse;
IMPORT Rd, Thread;
IMPORT AtomCellTbl;
IMPORT Subcell;

TYPE
  T = OBJECT
    cellTbl : AtomCellTbl.T;
    longNames : Subcell.LongNames;
  END;
  
PROCEDURE Parse(rd : Rd.T) : T
  RAISES { Rd.Failure, Thread.Alerted };
  
END BraceParse.
