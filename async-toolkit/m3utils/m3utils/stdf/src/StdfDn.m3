MODULE StdfDn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Word;
IMPORT StdfRd;
IMPORT Wx;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    u : [0..255];
    n : [0..65535];
  BEGIN
    IF len = 0 THEN RAISE StdfE.Missing END;

    n := StdfRd.U2(rd, len);
    t := NEW(T, n);
    
    FOR i := FIRST(t^) TO LAST(t^) DO
      IF i MOD 8 = 0 THEN
        u := StdfRd.U1(rd, len);
        t[i] := Word.And(u, 1) = 1
      ELSE
        t[i] := Word.And(Word.RightShift(u, i MOD 8), 1) = 1
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

PROCEDURE Default() : T = BEGIN RETURN NEW(T, 0) END Default;

BEGIN END StdfDn.
