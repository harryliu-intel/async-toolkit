(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Power;

FROM SvsTypes IMPORT CornerData;
IMPORT Math;
IMPORT Corner;
IMPORT Wx;
FROM Fmt IMPORT F, LongReal;
IMPORT SvsTypes;
IMPORT Debug;

CONST LR = LongReal;

PROCEDURE Calc(READONLY d  : Params;
               READONLY at : CornerData;
               doDebug     : BOOLEAN) : Result =
  BEGIN
    (* +sigma = fast, more leaky *)
    WITH RefRestPwr     = d.RefP - d.FixedP - d.RefLeakP,
         cornerLkgRatio = Math.pow(d.LkgRatio, at.sigma / d.LkgRatioSigma),
         ttV            = d.c[Corner.T.TT].vpower,
         voltPwrRatio   = (at.vpower/ttV)*(at.vpower/ttV),
         restPwr        = RefRestPwr * voltPwrRatio,
         leakPwr        = d.RefLeakP * cornerLkgRatio * voltPwrRatio,
         totPwr         = d.FixedP + restPwr + leakPwr DO
      IF doDebug THEN
        Debug.Out(F("Power.Calc: d.LkgRatio %s d.LkgRatioSigma %s vpower %s",
                    LR(d.LkgRatio), LR(d.LkgRatioSigma), LR(at.vpower)));
        Debug.Out(F("Power.Calc: d.RefLeakP %s sigma %s leakPwr %s",
                    LR(d.RefLeakP), LR(at.sigma), LR(leakPwr)))
      END;
      
      RETURN Result { cornerLkgRatio := cornerLkgRatio,
                      leakPwr        := leakPwr,
                      totPwr         := totPwr }
    END
  END Calc;

PROCEDURE FmtParams(READONLY p : Params) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    Wx.PutText(wx, F("RefP = %s\n",          LR(p.RefP)));
    Wx.PutText(wx, F("FixedP = %s\n",        LR(p.FixedP)));
    Wx.PutText(wx, F("RefLeakP = %s\n",      LR(p.RefLeakP)));
    Wx.PutText(wx, F("LkgRatio = %s\n",      LR(p.LkgRatio)));
    Wx.PutText(wx, F("LkgRatioSigma = %s\n", LR(p.LkgRatioSigma)));
    FOR i := FIRST(p.c) TO LAST(p.c) DO
      Wx.PutText(wx, F("Corner %s : %s\n",
                       Corner.Names[i],
                       SvsTypes.FmtCornerData(p.c[i])))
    END;
    RETURN Wx.ToText(wx)
  END FmtParams;

PROCEDURE FmtResult(READONLY r : Result) : TEXT =
  BEGIN
    RETURN F("Power.Result { cornerLkgRatio=%s leakPwr=%s totPwr=%s }",
             LR(r.cornerLkgRatio), LR(r.leakPwr), LR(r.totPwr))
  END FmtResult;
  
BEGIN END Power.
