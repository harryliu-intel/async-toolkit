MODULE MakeConstant;
IMPORT Wx;
IMPORT Text;
FROM Fmt IMPORT Int;

PROCEDURE CharsFromText(txt : TEXT) : TEXT =
  VAR
    wx := Wx.New();
    n  := Text.Length(txt);
  BEGIN
    Wx.PutText(wx, "ARRAY OF CHAR { ");
    FOR i := 0 TO n - 1 DO
      WITH c = Text.GetChar(txt, i) DO
        Wx.PutText(wx, "VAL(" & Int(ORD(c)) & ", CHAR)");
        IF i # n - 1 THEN
          Wx.PutText(wx, ", ")
        END
      END
    END;
    Wx.PutText(wx, " }");
    RETURN Wx.ToText(wx)
  END CharsFromText;

BEGIN END MakeConstant.
