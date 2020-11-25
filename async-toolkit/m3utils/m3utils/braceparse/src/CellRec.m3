MODULE CellRec;
IMPORT Wx;
FROM Fmt IMPORT Int;
IMPORT SubcellList;
IMPORT MosInfo;

PROCEDURE DebugOut(t : T; wx : Wx.T) =
  BEGIN
    Wx.PutText(wx, "CellRec \"" & t.nm & "\"\n");
    Wx.PutText(wx, Int(SubcellList.Length(t.subcells)));
    Wx.PutText(wx, " subcells:\n");
    Wx.PutText(wx, Int(t.mosTbl.size()));
    Wx.PutText(wx, " MOSFET types:\n");
    VAR
      iter := t.mosTbl.iterate();
      mosInfo : MosInfo.T;
      finCnt  : CARDINAL;
    BEGIN
      WHILE iter.next(mosInfo, finCnt) DO
        MosInfo.DebugOut(mosInfo, wx);
        Wx.PutInt(wx, finCnt);
        Wx.PutText(wx, " fins\n")
      END
    END
  END DebugOut;

BEGIN END CellRec.
