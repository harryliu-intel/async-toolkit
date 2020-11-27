INTERFACE BraceParse;
IMPORT Rd, Thread;
IMPORT AtomCellTbl;
IMPORT Subcell;
IMPORT OpenCharArrayRefTbl;

CONST BufSiz = 16*1024;

TYPE  Buffer = ARRAY [ 0 .. BufSiz-1 ] OF CHAR;

TYPE
  T = OBJECT
    cellTbl   : AtomCellTbl.T;
    longNames : Subcell.LongNames;
  END;
  
PROCEDURE Parse(rd : Rd.T; transistorCells : OpenCharArrayRefTbl.T) : T
  RAISES { Rd.Failure, Thread.Alerted };
  
END BraceParse.
