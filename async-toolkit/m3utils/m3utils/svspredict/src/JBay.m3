MODULE JBay;
FROM SvsTypes IMPORT CornerData;
IMPORT N7Tech AS Tech, N7PMRO AS TechPmro;
IMPORT PMRO;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT Power;
IMPORT Corner;
IMPORT Interpolate;
IMPORT Math;
IMPORT Svl;
IMPORT Scale;
IMPORT Wx;

CONST LR = Fmt.LongReal;

TYPE
  TvpMeasurement = RECORD
    t, v, p : LONGREAL;
  END;

PROCEDURE SetProgram(VAR p : Power.Params;
                     VAR Trunc                   : LONGREAL) =
  CONST
    f       = SetProgram79;
  BEGIN
    f(p, Trunc)
  END SetProgram;

PROCEDURE DebugPmroSigma(READONLY sigma : ARRAY Tech.Transistor OF LONGREAL) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    Wx.PutText(wx, "{");
    FOR i := FIRST(sigma) TO LAST(sigma) DO
      Wx.PutChar(wx, ' ');
      Wx.PutText(wx, Tech.TransistorNames[i]);
      Wx.PutText(wx, "=");
      Wx.PutText(wx, LR(sigma[i]))
    END;
    Wx.PutText(wx, " }");
    RETURN Wx.ToText(wx)
  END DebugPmroSigma;
  
PROCEDURE InterpolatePmro(READONLY specs : ARRAY Tech.Transistor OF PMRO.T;
                          READONLY meas  : ARRAY Tech.Transistor OF CARDINAL) :
  ARRAY Tech.Transistor OF LONGREAL =

  VAR
    res : ARRAY Tech.Transistor OF LONGREAL;
  BEGIN
    FOR i := FIRST(res) TO LAST(res) DO
      res[i] := Interpolate.OverCard(specs[i], Tech.CornerSigma, meas[i])
    END;
    RETURN res
  END InterpolatePmro;

CONST
  Pmro79  = ARRAY Tech.Transistor OF CARDINAL { 463, 422, 338 };
  Pmro139 = ARRAY Tech.Transistor OF CARDINAL { 487, 429, 332 };

PROCEDURE SetProgram79(VAR p     : Power.Params;
                       VAR Trunc : LONGREAL) =
  CONST
    pmro = Pmro79;
    (* PMRO data for this individual from the lab *)
    
    AteVmin79 = 645.0d-3;
    (* from Dinesh's estimations at ATE for this specific individual 
       updated per email 1/5/21 *)

    AgeMargin = 20.0d-3;

    CmvGuard  = 20.0d-3;

    tgtTemp = Svl.TargetTemp;
  VAR
    sigma79 := InterpolatePmro(TechPmro.V, pmro);
    (* this is now the leakage sigmas of #79 *)

    weightedSigma79 := WeightLkgSigma(sigma79);
    (* leakage weighted sigma *)

    speedSigma := sigma79[Tech.Transistor.Ulvt];
    (* take the geom. mean of the Ulvt sigma and the leakage sigma *)

    sigma139 := InterpolatePmro(TechPmro.V, Pmro139);
    
    weightedSigma139 := WeightLkgSigma(sigma139);
    (* weighted sigma *)
    
    Vmin79vsTT := Interpolate.OverLR(Tech.CornerSigma,
                                     Tech.SvsOffset,
                                     speedSigma);
    VminEolTT := AteVmin79 + AgeMargin - Vmin79vsTT + CmvGuard;

    (* now we will take data from chip 139 and massage them to stand in
       for measurements on 79 *)

    pred139 := PredPower(MeasAct139, MeasLkg139, VminEolTT, tgtTemp);
    (* bring the power figures of 139 to the conditions we care about *)

    leakRatio139 := Interpolate.Exp(Tech.CornerSigma,
                                    Tech.CornerLkgRatio,
                                    weightedSigma139);

    leakPwrTt := pred139.lkgP / leakRatio139;

    predTt := Power3 { dynP := pred139.dynP,
                       lkgP := leakPwrTt,
                       totP := pred139.dynP + leakPwrTt };
  BEGIN
    Debug.Out(F("SetProgram79 : sigma79 = %s, weightedSigma79 = %s, speedSigma = %s",
                DebugPmroSigma(sigma79), LR(weightedSigma79), LR(speedSigma)));
    Debug.Out(F("SetProgram79 : Vmin79vsTT = %s, VminEolTT = %s",
                LR(Vmin79vsTT), LR(VminEolTT)));
    Debug.Out(F("SetProgram79 : pred139 = %s",
                DebugPower3(pred139)));
    Debug.Out(F("SetProgram79 : leakRatio139 = %s, leakPwrTt = %s",
                LR(leakRatio139), LR(leakPwrTt)));
    Debug.Out(F("SetProgram79 : predTt = %s",
                DebugPower3(predTt)));

    SetShared(p, Trunc);
    p.c             := MakeCornerArrayFromTt(VminEolTT);
    p.RefLeakP      := predTt.lkgP;
    p.RefP          := predTt.totP;
  END SetProgram79;

PROCEDURE SetShared(VAR p : Power.Params; VAR Trunc : LONGREAL) =
  BEGIN
    p.LkgRatio      :=   Tech.CornerLkgRatio[Corner.T.FF] /
                         Tech.CornerLkgRatio[Corner.T.TT]; 
    p.LkgRatioSigma :=   Tech.CornerSigma[Corner.T.FF];
    Trunc           :=   Tech.CornerSigma[Corner.T.FF];

    p.FixedP        := 107.0d0;
  END SetShared;
  
PROCEDURE MakeCornerArrayFromTt(vminEolTt : LONGREAL) : ARRAY Corner.T OF CornerData =
  VAR
    TtPowV := vminEolTt;
    FfPowV := TtPowV + Tech.SvsOffset[Corner.T.FF];
    SsPowV := TtPowV + Tech.SvsOffset[Corner.T.SS];
  BEGIN
    RETURN ARRAY Corner.T OF CornerData {
    CornerData { SsPowV + PowToContact, SsPowV, Tech.CornerSigma[Corner.T.SS] },
    CornerData { TtPowV + PowToContact, TtPowV, Tech.CornerSigma[Corner.T.TT] },
    CornerData { FfPowV + PowToContact, FfPowV, Tech.CornerSigma[Corner.T.FF] }
    };
    
  END MakeCornerArrayFromTt;
  
CONST
  Pmro242    = ARRAY Tech.Transistor OF CARDINAL { 495, 445, 359 };
  (* PMRO 242 likely measured under the wrong conditions -- DO NOT TRUST! *)

  MeasLkg242 = TvpMeasurement { 95.0d0, 0.750d0,  33.188d0 };

  MeasAct242 = TvpMeasurement { 96.0d0, 0.700d0, 336.0d0   };

  MeasLkg139 = TvpMeasurement { 90.0d0, 0.735d0,  22.964d0 };

  MeasAct139 = TvpMeasurement { 45.0d0, 0.730d0, 342.06d0  };
  (* this includes leakage *)

PROCEDURE WeightLkgSigma(sigma : ARRAY Tech.Transistor OF LONGREAL) : LONGREAL =
  VAR
    sumWeight, sumSigma := 0.0d0;
  BEGIN
    FOR i := FIRST(sigma) TO LAST(sigma) DO
      WITH weight = FinCounts[i] * Tech.TranLeakageRatio[i] DO
        sumWeight := sumWeight + weight;
        sumSigma  := sumSigma  + weight * sigma[i]
      END;
      Debug.Out(F("transistor type %4s sigma %s",
                  Tech.TransistorNames[i], LR(sigma[i])))
    END;

    RETURN sumSigma / sumWeight;
  END WeightLkgSigma;

PROCEDURE SetProgram242(<*NOWARN*>VAR p : Power.Params;
                        <*NOWARN*>VAR Trunc                   : LONGREAL) =
  CONST
    pmro    = Pmro242;
  VAR
    sigma := InterpolatePmro(TechPmro.V, pmro);
    weightedSigma := WeightLkgSigma(sigma);
  BEGIN
    Debug.Out(F("weighted sigma %s",
                LR(weightedSigma)));
    (* not done yet *)
    <*ASSERT FALSE*>
  END SetProgram242;
  
PROCEDURE SetProgram139(VAR p                       : Power.Params;
                        VAR Trunc                   : LONGREAL) =
  CONST
    (* endless discussions 12/3/20 *)

    LabVmin139    =  0.715d0;
    AgeMargin     =  0.020d0;
    Vmin139vsFF   = +10.22d-3;
    VminEolFF     =  LabVmin139 + AgeMargin - Vmin139vsFF;

    FfPowV        =  VminEolFF;
    TtPowV        =  FfPowV - Tech.SvsOffset[Corner.T.FF];
    SsPowV        =  TtPowV + Tech.SvsOffset[Corner.T.SS];

    ThisLeakSigma = +2.2335d0; (* + for fast *)
  BEGIN
    SetShared(p, Trunc);
    
    p.c := ARRAY Corner.T OF CornerData {
    CornerData { SsPowV + PowToContact, SsPowV, Tech.CornerSigma[Corner.T.SS] },
    CornerData { TtPowV + PowToContact, TtPowV, Tech.CornerSigma[Corner.T.TT] },
    CornerData { FfPowV + PowToContact, FfPowV, Tech.CornerSigma[Corner.T.FF] }
    };
    
    (* from lab measurements : we assumed measured chip is at FF *)
    (* These figures are re-computed for a TT chip *)

    p.RefP          := 377.90d0;

    (* 
       arrived at by trial and error, I think : we are trying to hit the 
       correct power for the voltage corresponding to the observed power of
       #139 in the lab.  

       Stats on #139:
       1.5GHz / 1.3125GHz / 730mV / 45C:     354.96W
       leakage                                -6   
       ->1.3125^2                             -5
       MAU fuse                               -5
       lower SERDES Vdd                       -3.2
      ----------------------------------------------
       #139 Dyn @ 730mV                      336.06W
      ----------------------------------------------
       @735mV                                340.68W
       Lkg 22.964@90C -> @95C :               26.41
      ----------------------------------------------
       tot power @ 95C/735mV                 367.09W
    *)
    
    WITH leakFactor = Math.pow(p.LkgRatio, ThisLeakSigma / p.LkgRatioSigma) DO
      p.RefLeakP      := 26.41d0 / leakFactor
    END
  END SetProgram139;

TYPE
  Power3 = RECORD
    dynP, lkgP, totP : LONGREAL;
  END;

PROCEDURE DebugPower3(READONLY p3 : Power3) : TEXT =
  BEGIN
    RETURN F("Power3 { dynP = %s, lkgP = %s, totP = %s }",
             LR(p3.dynP), LR(p3.lkgP), LR(p3.totP))
  END DebugPower3;
  
PROCEDURE FmtTvpMeasurement(READONLY q : TvpMeasurement) : TEXT =
  BEGIN
    RETURN F("Tvp { t = %s, v = %s, p = %s }",
             LR(q.t), LR(q.v), LR(q.p))
  END FmtTvpMeasurement;
  
PROCEDURE PredPower(measAct    : TvpMeasurement;
                    measLkg    : TvpMeasurement;
                    reqV, reqT : LONGREAL) : Power3 =
  VAR
    lkgAtAct := measLkg.p *
                Scale.LkgPwrByT(measLkg.t, measAct.t) * 
                Scale.LkgPwrByV(measLkg.v, measAct.v);
    (* leakage power predicted for conditions seen at actual measurement *)

    dynAtAct := measAct.p - lkgAtAct;
    
    lkgAtReq := measLkg.p *
                Scale.LkgPwrByT(measLkg.t, reqT) * 
                Scale.LkgPwrByV(measLkg.v, reqV);
    
    dynAtReq := dynAtAct *
                Scale.DynPwrByT(measAct.t, reqT) * 
                Scale.DynPwrByV(measAct.v, reqV);
  BEGIN
    Debug.Out(F("PredPower measLkg = %s", FmtTvpMeasurement(measLkg)));
    Debug.Out(F("PredPower measAct = %s", FmtTvpMeasurement(measAct)));
    Debug.Out(F("PredPower reqV = %s, reqT = %s", LR(reqV), LR(reqT)));
    Debug.Out(F("lkgAtAct = %s, dynAtAct = %s, lkgAtReq = %s, dynAtReq = %s",
                LR(lkgAtAct), LR(dynAtAct), LR(lkgAtReq), LR(dynAtReq)));
    
    RETURN Power3 { dynAtReq, lkgAtReq, dynAtReq + lkgAtReq }
  END PredPower;

BEGIN END JBay.
