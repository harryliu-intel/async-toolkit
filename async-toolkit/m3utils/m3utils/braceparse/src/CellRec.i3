INTERFACE CellRec;
IMPORT Subcell;
IMPORT MosInfoCardTbl;
IMPORT Wx;

TYPE
  T = OBJECT
    nm       : TEXT;
    subcells : REF ARRAY OF Subcell.T;
    mosTbl   : MosInfoCardTbl.T;
  END;

CONST Brand = "CellRec";

PROCEDURE DebugOut(t : T; wx : Wx.T);

END CellRec.
    
