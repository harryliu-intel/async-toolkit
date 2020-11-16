MODULE StdfDn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Word;
IMPORT StdfC1;
IMPORT Wx;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    c : ARRAY [0..0] OF CHAR;
  BEGIN
    FOR i := FIRST(t^) TO LAST(t^) DO
      IF i MOD 8 = 0 THEN
        StdfC1.Parse(rd, len, c);
        t[i] := Word.And(ORD(c[0]),1) = 1
      ELSE
        t[i] := Word.And(Word.RightShift(ORD(c[0]),i MOD 8), 1) = 1
      END
    END
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(t^) TO LAST(t^) DO
      IF t[i] THEN
        Wx.PutChar(wx, 't')
      ELSE
        Wx.PutChar(wx, 'f')
      END
    END;
    RETURN Brand & " : " & Wx.ToText(wx)
  END Format;

BEGIN END StdfDn.
