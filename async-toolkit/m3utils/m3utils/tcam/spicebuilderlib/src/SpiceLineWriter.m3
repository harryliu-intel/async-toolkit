MODULE SpiceLineWriter;
IMPORT Wr, Thread, Wx;
IMPORT Text;

REVEAL
  T = Public BRANDED Brand OBJECT 
    wr : Wr.T;
    wx : Wx.T;
    max : CARDINAL;
    wl := 0;
  OVERRIDES
    init := Init;
    word := Word;
    eol  := EOL;
    pl   := PL;
  END;

PROCEDURE PL(t : T; txt : TEXT) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN Wr.PutText(t.wr, txt); Wr.PutChar(t.wr, '\n') END PL;

PROCEDURE Init(t : T; wr : Wr.T; max : INTEGER) : T =
  BEGIN
    t.max := max;
    t.wr := wr;
    t.wx := Wx.New();
    RETURN t
  END Init;

PROCEDURE Word(t : T; w : TEXT) RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    delta := Text.Length(w) + 1;
  BEGIN
    IF t.wl + delta > t.max THEN
      t.pl(Wx.ToText(t.wx));
      Wx.Reset(t.wx);
      Wx.PutChar(t.wx, '+');
      Wx.PutText(t.wx, w);
      t.wl := delta
    ELSE
      IF t.wl # 0 THEN Wx.PutChar(t.wx, ' ') END;
      Wx.PutText(t.wx, w);
      t.wl := t.wl + delta
    END;
  END Word;
  
PROCEDURE EOL(t : T) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    t.pl(Wx.ToText(t.wx));
    Wx.Reset(t.wx);
    t.wl := 0
  END EOL;
  
BEGIN END SpiceLineWriter.
