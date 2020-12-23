MODULE Power;

FROM SvsTypes IMPORT CornerData;
IMPORT Math;
IMPORT Corner;
IMPORT Wx;
FROM Fmt IMPORT F, LongReal;

CONST LR = LongReal;

PROCEDURE Calc(READONLY d : Params; at : CornerData) : Result =
  BEGIN
    (* +sigma = slow, less leaky *)
    WITH RefRestPwr     = d.RefP - d.FixedP - d.RefLeakP,
         cornerLkgRatio = Math.pow(d.LkgRatio, -at.sigma / d.LkgRatioSigma),
         ttV            = d.c[Corner.T.TT].vpower,
         voltPwrRatio   = (at.vpower/ttV)*(at.vpower/ttV),
         restPwr        = RefRestPwr * voltPwrRatio,
         leakPwr        = d.RefLeakP * cornerLkgRatio * voltPwrRatio,
         totPwr         = d.FixedP + restPwr + leakPwr DO
      RETURN Result { cornerLkgRatio := cornerLkgRatio,
                      leakPwr        := leakPwr,
                      totPwr         := totPwr }
    END
  END Calc;

PROCEDURE FmtParams(READONLY p : Params) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    Wx.PutText(wx, F("RefP = %s\n", LR(p.RefP)));
    Wx.PutText(wx, F("FixedP = %s\n", LR(p.FixedP)));
    Wx.PutText(wx, F("RefLeakP = %s\n", LR(p.RefLeakP)));
    Wx.PutText(wx, F("LkgRatio = %s\n", LR(p.LkgRatio)));
    Wx.PutText(wx, F("LkgRatioSigma = %s\n", LR(p.LkgRatioSigma)));
    RETURN Wx.ToText(wx)
  END FmtParams;
  
BEGIN END Power.
