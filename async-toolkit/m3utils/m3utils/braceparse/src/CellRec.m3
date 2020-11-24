MODULE CellRec;
IMPORT Wx;
FROM Fmt IMPORT Int;
IMPORT SubcellList;

PROCEDURE DebugOut(t : T; wx : Wx.T) =
  BEGIN
    Wx.PutText(wx, "CellRec \"" & t.nm & "\"\n");
    Wx.PutText(wx, Int(SubcellList.Length(t.subcells)));
    Wx.PutText(wx, " subcells:\n");
    Wx.PutText(wx, Int(t.mosTbl.size()));
    Wx.PutText(wx, " MOSFETs:\n");
  END DebugOut;

BEGIN END CellRec.
