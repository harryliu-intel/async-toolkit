MODULE CellRec;
IMPORT Wx;
FROM Fmt IMPORT Int;
IMPORT MosInfo;
IMPORT CardPair;

PROCEDURE DebugOut(t : T; wx : Wx.T) =
  BEGIN
    Wx.PutText(wx, "CellRec \"" & t.nm & "\"\n");
    Wx.PutText(wx, Int(NUMBER(t.subcells^)));
    Wx.PutText(wx, " subcells:\n");
    Wx.PutText(wx, Int(t.mosTbl.size()));
    Wx.PutText(wx, " MOSFET types:\n");
    VAR
      iter := t.mosTbl.iterate();
      mosInfo : MosInfo.T;
      finCnt  : CardPair.T;
    BEGIN
      WHILE iter.next(mosInfo, finCnt) DO
        MosInfo.DebugOut(mosInfo, wx);
        Wx.PutInt(wx, finCnt.k2);
        Wx.PutText(wx, " devices ");
        Wx.PutInt(wx, finCnt.k2);
        Wx.PutText(wx, " fins\n")
      END
    END
  END DebugOut;

BEGIN END CellRec.
