INTERFACE CellRec;
IMPORT SubcellList;
IMPORT MosInfoCardTbl;
IMPORT Wx;

TYPE
  T = OBJECT
    nm       : TEXT;
    subcells : SubcellList.T;
    mosTbl   : MosInfoCardTbl.T;
  END;

CONST Brand = "CellRec";

PROCEDURE DebugOut(t : T; wx : Wx.T);

END CellRec.
    
