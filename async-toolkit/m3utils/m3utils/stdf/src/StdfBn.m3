MODULE StdfBn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Word;
IMPORT StdfRd;
IMPORT Wx;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    u, v : [0..255];
  BEGIN
    IF len = 0 THEN RAISE StdfE.Missing END;
    u := StdfRd.U1(rd, len);
    t := NEW(REF ARRAY OF BOOLEAN, 8 * u);
    FOR i := FIRST(t^) TO LAST(t^) DO
      IF i MOD 8 = 0 THEN
        v  := StdfRd.U1(rd, len);
        t[i] := Word.And(v,1) = 1
      ELSE
        t[i] := Word.And(Word.RightShift(v, i MOD 8), 1) = 1
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

BEGIN END StdfBn.
