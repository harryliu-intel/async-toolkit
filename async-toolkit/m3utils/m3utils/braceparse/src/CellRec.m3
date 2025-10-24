MODULE CellRec;
IMPORT CellRecClass;
IMPORT Wx;
FROM Fmt IMPORT Int;
IMPORT MosInfo;
IMPORT FinInfo;
IMPORT Atom;

REVEAL
  T = CellRecClass.Private BRANDED Brand OBJECT END;

PROCEDURE DebugOut(t : T; wx : Wx.T) =
  BEGIN
    Wx.PutText(wx, "CellRec \"" & Atom.ToText(t.nm) & "\"\n");
    Wx.PutText(wx, Int(NUMBER(t.subcells^)));
    Wx.PutText(wx, " subcells:\n");
    Wx.PutText(wx, Int(t.mosTbl.size()));
    Wx.PutText(wx, " MOSFET types:\n");
    VAR
      iter := t.mosTbl.iterate();
      mosInfo : MosInfo.T;
      finCnt  : FinInfo.T;
    BEGIN
      WHILE iter.next(mosInfo, finCnt) DO
        MosInfo.DebugOut(mosInfo, wx);
        Wx.PutInt(wx, finCnt[FinInfo.Info.MosCnt]);
        Wx.PutText(wx, " devices ");
        Wx.PutInt(wx, finCnt[FinInfo.Info.FinCnt]);
        Wx.PutText(wx, " fins\n")
      END
    END
  END DebugOut;

BEGIN END CellRec.
