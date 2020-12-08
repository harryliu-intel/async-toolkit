INTERFACE BraceParse;
IMPORT Rd, Thread;
IMPORT AtomCellTbl;
IMPORT OpenCharArrayRefTbl;
IMPORT LongNames;
IMPORT DrawnWidth;

CONST BufSiz = 16*1024;

TYPE  Buffer = ARRAY [ 0 .. BufSiz-1 ] OF CHAR;

TYPE
  T = OBJECT
    cellTbl   : AtomCellTbl.T;
    longNames : LongNames.T;
  END;
  
PROCEDURE Parse(rd : Rd.T;
                transistorCells : OpenCharArrayRefTbl.T;
                drawnWidth : DrawnWidth.T := NIL) : T
  RAISES { Rd.Failure, Thread.Alerted };

PROCEDURE InitCellTblAux(tbl : AtomCellTbl.T; to : CARDINAL);
  
END BraceParse.
