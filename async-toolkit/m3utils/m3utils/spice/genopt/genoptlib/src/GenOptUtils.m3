MODULE GenOptUtils;
IMPORT LRVector, Wx;
FROM Fmt IMPORT LongReal;
IMPORT Pathname;
IMPORT Wr;
IMPORT LongRealSeq AS LRSeq;
IMPORT OSError;
IMPORT AL;
IMPORT FileWr;
FROM Fmt IMPORT F;
IMPORT Debug;

PROCEDURE FmtP(p : LRVector.T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO
      Wx.PutText(wx, LongReal(p[i]));
      Wx.PutChar(wx, ' ')
    END;
    RETURN Wx.ToText(wx)
  END FmtP;

PROCEDURE MustOpenWr(pn : Pathname.T) : Wr.T =
  BEGIN
    TRY
      RETURN FileWr.Open(pn)
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Couldnt open %s : OSError.E : %s",
                    pn, AL.Format(x)));
      <*ASSERT FALSE*>
    END
  END MustOpenWr;

PROCEDURE LRSeq1(x : LONGREAL) : LRSeq.T =
  BEGIN
    WITH res = NEW(LRSeq.T).init() DO
      res.addhi(x);
      RETURN res
    END
  END LRSeq1;

PROCEDURE FmtLRSeq(seq : LRSeq.T) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      res := res & LongReal(seq.get(i)) & " "
    END;
    RETURN res
  END FmtLRSeq;
  
BEGIN END GenOptUtils.
