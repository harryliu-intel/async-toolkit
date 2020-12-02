MODULE StdfB1;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Word;
IMPORT StdfRd;
IMPORT Wx;
IMPORT Wr;
IMPORT StdfWr;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    u : [0..255];
  BEGIN
    IF len = 0 THEN RETURN END;
    FOR i := FIRST(t) TO LAST(t) DO
      IF i MOD 8 = 0 THEN
        u := StdfRd.U1(rd, len);
        t[i] := Word.And(u,1) = 1
      ELSE
        t[i] := Word.And(Word.RightShift(u, i MOD 8), 1) = 1
      END
    END
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(t) TO LAST(t) DO
      IF t[i] THEN
        Wx.PutChar(wx, 't')
      ELSE
        Wx.PutChar(wx, 'f')
      END
    END;
    RETURN Brand & " : " & Wx.ToText(wx)
  END Format;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Wr.Failure, Thread.Alerted } =
  CONST
    Sel = ARRAY [FALSE..TRUE] OF CARDINAL { 0, 1 };
  VAR
    b : [0..255] := 0;
    k := 0;
  BEGIN
    FOR i := 0 TO 7 DO
      b := Word.Insert(b, Sel[t[i]], k, 1);
      INC(k)
    END;
    StdfWr.U1(wr, b)
  END Write;
  
BEGIN END StdfB1.
