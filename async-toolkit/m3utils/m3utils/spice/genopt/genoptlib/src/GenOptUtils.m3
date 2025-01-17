MODULE GenOptUtils;
IMPORT LRVector, Wx;
FROM Fmt IMPORT LongReal, Style;
IMPORT Pathname;
IMPORT Wr;
IMPORT LongRealSeq AS LRSeq;
IMPORT OSError;
IMPORT AL;
IMPORT FileWr;
FROM Fmt IMPORT F;
IMPORT Debug;
IMPORT LRVectorSeq;

PROCEDURE FmtP(p : LRVector.T; style : Style; prec : CARDINAL) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO
      Wx.PutText(wx, LongReal(p[i], style, prec));
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

PROCEDURE FmtLRSeq(seq : LRSeq.T; style : Style; prec : CARDINAL) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      res := res & LongReal(seq.get(i), style, prec) & " "
    END;
    RETURN res
  END FmtLRSeq;
  
PROCEDURE FmtLRVectorSeq(seq : LRVectorSeq.T; style : Style; prec : CARDINAL) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      res := res & "{" & FmtP(seq.get(i), style, prec) & "} "
    END;
    RETURN res
  END FmtLRVectorSeq;

PROCEDURE LRVectorSeq1(x : LONGREAL; dims : CARDINAL) : LRVectorSeq.T =
  VAR
    v := NEW(LRVector.T, dims);
  BEGIN
    FOR i := FIRST(v^) TO LAST(v^) DO
      v[i] := x
    END;
    
    WITH res = NEW(LRVectorSeq.T).init() DO
      res.addhi(v);
      RETURN res
    END
  END LRVectorSeq1;

BEGIN END GenOptUtils.
