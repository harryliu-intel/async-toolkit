MODULE JBay;
FROM SvsTypes IMPORT CornerData;
IMPORT N7Tech AS Tech, N7PMRO AS TechPmro;
IMPORT PMRO;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F, FN;
IMPORT Power;
IMPORT Corner;
IMPORT Interpolate;
IMPORT Math;
IMPORT Svl;
IMPORT Scale;
IMPORT Wx;
IMPORT TvpMeasurement;
IMPORT Power3;
IMPORT PowerScaling;
IMPORT Wr;
IMPORT NullWr;

CONST LR = Fmt.LongReal;

CONST FixedDynPwr = 107.0d0;

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
  Pmro222 = ARRAY Tech.Transistor OF CARDINAL { 503, 454, 365 };

PROCEDURE SetProgram79(VAR p     : Power.Params;
                       VAR Trunc : LONGREAL) =
  CONST
    pmro = Pmro79;
    (* PMRO data for this individual from the lab *)
    
    (*AteVmin79   = 670.0d-3;*)
    AteVmin79 = 645.0d-3;
    (* from Dinesh's estimations at ATE for this specific individual 
       updated per email 1/5/21 : 645mV *)

    AgeMargin = 15.0d-3; (*20.0d-3;*)

    CmvGuard  = 10.0d-3; (*20.0d-3;*)

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

    pred139 := PowerScaling.Predict(MeasAct139, 
                                    MeasLkg139,
                                    VminEolTT,
                                    tgtTemp,
                                    FixedDynPwr);
    (* bring the power figures of 139 to the conditions we care about *)

    leakRatio139 := Interpolate.Exp(Tech.CornerSigma,
                                    Tech.CornerLkgRatio,
                                    weightedSigma139);

    leakPwrTt := pred139.lkgP / leakRatio139 * Tech.CornerLkgRatio[Corner.T.TT];

    predTt := Power3.T { dynP := pred139.dynP,
                         lkgP := leakPwrTt,
                         totP := pred139.dynP + leakPwrTt };
  BEGIN
    Debug.Out(F("SetProgram79 : sigma79 = %s, weightedSigma79 = %s, speedSigma = %s",
                DebugPmroSigma(sigma79), LR(weightedSigma79), LR(speedSigma)));
    Debug.Out(F("SetProgram79 : Vmin79vsTT = %s, VminEolTT = %s",
                LR(Vmin79vsTT), LR(VminEolTT)));
    Debug.Out(F("SetProgram79 : pred139 = %s",
                Power3.DebugFmt(pred139)));
    Debug.Out(F("SetProgram79 : leakRatio139 = %s, leakPwrTt = %s",
                LR(leakRatio139), LR(leakPwrTt)));
    Debug.Out(F("SetProgram79 : predTt = %s",
                Power3.DebugFmt(predTt)));

    SetShared(p, Trunc);
    p.c             := MakeCornerArrayFromTt(VminEolTT);
    p.RefLeakP      := predTt.lkgP;
    p.RefP          := predTt.totP;

  END SetProgram79;

CONST
  MacCredit    = -6.0d0; (* from Remy  *)
  SerdesMargin = +3.0d0; (* Dan's est. *)
  Corr         = MacCredit + SerdesMargin;
  WorV         = 0.04d0;
  TypV         = 0.02d0;

PROCEDURE EvalCustomerMaxSpec(READONLY p : Power.Params;
                              worstSigma : LONGREAL) : LONGREAL =
  VAR
    wr := NEW(NullWr.T).init();
  BEGIN
    RETURN Conditions(wr, p, 105.0d0, 1.0d0, 300.0d0, worstSigma,  Corr, WorV);
  END EvalCustomerMaxSpec;
  
PROCEDURE EvalSpecialCases(wr : Wr.T;
                           READONLY p : Power.Params;
                           medianSigma, worstSigma : LONGREAL) =
  BEGIN
    ConditionsHeader(wr);
    EVAL Conditions(wr, p, 105.0d0, 1.0d0, 300.0d0, worstSigma,  Corr, WorV);
    EVAL Conditions(wr, p, 105.0d0, 1.0d0, 300.0d0, medianSigma, Corr, WorV);
    EVAL Conditions(wr, p, 105.0d0, 1.0d0, 300.0d0, medianSigma, Corr, 0.0d0);
    EVAL Conditions(wr, p, 105.0d0, 1.0d0, 380.0d0, medianSigma, Corr, 0.0d0);

    EVAL Conditions(wr, p,  90.0d0, 1.0d0, 300.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  90.0d0, 0.7d0, 300.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  90.0d0, 0.3d0, 300.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  75.0d0, 0.3d0, 300.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  75.0d0, 0.0d0, 300.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  75.0d0, 0.0d0,9000.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  90.0d0, 1.0d0,9000.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  90.0d0, 0.0d0,9000.0d0, medianSigma, Corr, TypV);

    EVAL Conditions(wr, p,  90.0d0, 0.0d0,9000.0d0, 2.23d0, 0.0d0, 0.113d0);

    (* the following are numbers for Google on CB -- just to test *)

    EVAL Conditions(wr, p, 105.0d0, 1.0d0, 300.0d0, worstSigma,  Corr, TypV);
    EVAL Conditions(wr, p, 105.0d0, 1.0d0, 300.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  95.0d0, 1.0d0, 300.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  95.0d0, 1.0d0,1000.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  95.0d0, 0.7d0,1000.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  95.0d0, 0.5d0,1000.0d0, medianSigma, Corr, TypV);
    EVAL Conditions(wr, p,  95.0d0, 0.3d0,1000.0d0, medianSigma, Corr, TypV);

    (* try to replicate lab idle measurement of indiv #139 *)
  END EvalSpecialCases;

PROCEDURE ConditionsHeader(wr : Wr.T) =
  BEGIN
    Wr.PutText(wr, "temp, active, bytes, sigma, deltaP, voltFrac, chipVoltMin, vrVolt, power\n");
  END ConditionsHeader;

PROCEDURE Conditions(wr            : Wr.T;
                     READONLY p    : Power.Params;
                     (* p is for a chip at Svl.TargetTemp and TT per
                        the corner data *)
                     temp,
                     active,
                     bytes,
                     sigma,
                     deltaP, (* +debit / -credit on power *)
                     voltMarginFrac : LONGREAL) : LONGREAL =
  CONST
    ActiveAt300B = 0.2943d0; (* percentage of non-leakage non-fixed power
                                from packet size *)

    (* this number DOES NOT match Raji's lab data at all.  Her lab data 
       suggests it ought to be much bigger. *)
  VAR
    activeAtBytes     := ActiveAt300B / bytes * 300.0d0;

    bytesFracOfActive := 1.0d0 - ActiveAt300B + activeAtBytes;

    q := p;

    ttActMeasurement := TvpMeasurement.T { Svl.TargetTemp,
                                           p.c[Corner.T.TT].vpower,
                                           p.RefP };
    
    ttLkgMeasurement := TvpMeasurement.T { Svl.TargetTemp,
                                           p.c[Corner.T.TT].vpower,
                                           p.RefLeakP };

    ttAtTemp : Power3.T;
    powerResultsNoMargin : Power.Result;
    cd : CornerData;
    chipVoltReq, vrVoltAssumed : LONGREAL;

  BEGIN
    Debug.Out(FN("*** Conditions temp %s active %s bytes %s sigma %s deltaP %s voltMargin %s",
                 ARRAY OF TEXT {
    LR(temp),
    LR(active),
    LR(bytes),
    LR(sigma),
    LR(deltaP),
    LR(voltMarginFrac)}));

    
    ttAtTemp := PowerScaling.Predict(ttActMeasurement,
                                     ttLkgMeasurement,
                                     p.c[Corner.T.TT].vpower,
                                     temp,
                                     FixedDynPwr);
    cd := PowerScaling.Interpolate(q, sigma); (* corner data for new *)
    q.RefP     := ttAtTemp.totP;
    q.RefLeakP := ttAtTemp.lkgP;

    powerResultsNoMargin := Power.Calc(q, cd, TRUE);


    Debug.Out("TT params @ ref T/V " & Power.FmtParams(p));
    Debug.Out("TT params @ tgt T   " & Power.FmtParams(q));
    Debug.Out(F("@ sigma=%s vpower=%s", LR(sigma), LR(cd.vpower)));
    Debug.Out(Power.FmtResult(powerResultsNoMargin));

    VAR
      cdMargin := cd;
      powerResultsMargin : Power.Result;

      creditedPwr : LONGREAL;
      nonFixedNonLeakPwr : LONGREAL;
      varPwr, bytesPwr, activePwr : LONGREAL;
    BEGIN
      cdMargin.vpower := (1.0d0 + voltMarginFrac) * cd.vpower;
      powerResultsMargin := Power.Calc(q, cdMargin, TRUE);
      Debug.Out(F("@ sigma=%s vpower=%s", LR(sigma), LR(cdMargin.vpower)));

      chipVoltReq   := cd.vpower;
      vrVoltAssumed := cdMargin.vpower;
      
      Debug.Out(Power.FmtResult(powerResultsMargin));

      creditedPwr := powerResultsMargin.totPwr + deltaP;

      Debug.Out("with final credits/debits P=" & LR(creditedPwr));

      VAR
        (* now apply activity *)
        nonFixedNonLeakPwr :=
            creditedPwr - powerResultsMargin.leakPwr - FixedDynPwr;
        
        potBytesPwr := nonFixedNonLeakPwr * ActiveAt300B;
        
        sizedBytesPwr    := potBytesPwr * 300.0d0 / bytes;
        
        nonBytesFrac      := (activeFrac - ActiveAt300B);
        
        potNonBytesPwr       := nonBytesFrac * nonFixedNonLeakPwr;
        
        activeActualBytesPwr := active * sizedBytesPwr;
        activeNonBytesPwr    := active * potNonBytesPwr;
        
        totVarPower          := activeActualBytesPwr + activeNonBytesPwr;
        
        potVarPower          := potBytesPwr + potNonBytesPwr;
        
        finalPwr := creditedPwr - potVarPower + totVarPower;

      BEGIN
        Debug.Out(F("non-fixed-non-leak power   %s", LR(nonFixedNonLeakPwr)));
        Debug.Out(F("potential bytes power      %s", LR(potBytesPwr)));
        Debug.Out(F("sized bytes power          %s", LR(sizedBytesPwr)));
        Debug.Out(F("active bytes power         %s", LR(activeActualBytesPwr)));
        Debug.Out(F("potential non-bytes power  %s", LR(potNonBytesPwr)));
        Debug.Out(F("totVarPower                %s", LR(totVarPower)));
        Debug.Out(F("potVarPower                %s", LR(potVarPower)));

        Debug.Out(F("final power   %s", LR(finalPwr)));

        Wr.PutText(wr, FN("%s, %s, %s, %s, %s, %s, %s, %s, %s\n",     ARRAY OF TEXT {
        LR(temp),
        LR(active),
        LR(bytes),
        LR(sigma),
        LR(deltaP),
        LR(voltMarginFrac),
        LR(chipVoltReq),
        LR(vrVoltAssumed),
        LR(finalPwr)}));
        
        RETURN finalPwr
      END
    END
    
  END Conditions;

VAR
  activeFrac : LONGREAL;
  (* this is the fraction of the non-leakage non-fixed power that
     varies with traffic (total traffic variation) 
     
     i.e., if the system is idle, take off this much of the non-leakage 
     non-fixed power 

     It is initialized by EvalActiveMinusIdle below.
  *)
  
    
PROCEDURE EvalActiveMinusIdle() =
  CONST
    T = 90.0d0;
    V = 0.730d0;
    
  VAR
    actPred := PowerScaling.Predict(MeasAct139, MeasLkg139, V, T, FixedDynPwr);
    idlPred := PowerScaling.Predict(MeasIdl139, MeasLkg139, V, T, FixedDynPwr);

    varP    := actPred.dynP - idlPred.dynP;

    varFrac := varP / (actPred.dynP - FixedDynPwr);
    (* this is the varying part of the power as a fraction of
       the non-leakage non-fixed (serdes) power *)
    
  BEGIN
    Debug.Out("Active      " & Power3.DebugFmt(actPred));
    Debug.Out("Idle        " & Power3.DebugFmt(idlPred));
    Debug.Out("Variable    " & LR(varP));
    Debug.Out("frac of dyn " & LR(varFrac));
    activeFrac := varFrac;
  END EvalActiveMinusIdle;

                         
PROCEDURE SetShared(VAR p : Power.Params; VAR Trunc : LONGREAL) =
  BEGIN
    p.LkgRatio      :=   Tech.CornerLkgRatio[Corner.T.FF] /
                         Tech.CornerLkgRatio[Corner.T.TT]; 
    p.LkgRatioSigma :=   Tech.CornerSigma[Corner.T.FF];
    Trunc           :=   Tech.CornerSigma[Corner.T.FF];

    p.FixedP        :=   FixedDynPwr;
  END SetShared;
  
PROCEDURE MakeCornerArrayFromTt(vminEolTt : LONGREAL) :
  ARRAY Corner.T OF CornerData =
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

  MeasLkg242 = TvpMeasurement.T { 95.0d0, 0.750d0,  33.188d0 };

  MeasAct242 = TvpMeasurement.T { 96.0d0, 0.700d0, 336.0d0   };

  MeasLkg139 = TvpMeasurement.T { 90.0d0, 0.735d0,  22.964d0 };

  MeasAct139 = TvpMeasurement.T { 45.0d0, 0.730d0, 342.06d0  };
  (* this includes leakage, 300B frames *)

  MeasIdl139 = TvpMeasurement.T { 90.0d0, 0.750d0, 261.072d0 };
  (* idle power *)



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

BEGIN
  EvalActiveMinusIdle();

  WITH sigma222 = InterpolatePmro(TechPmro.V, Pmro222),
       speedSigma = sigma222[Tech.Transistor.Ulvt],
       weightedSigma = WeightLkgSigma(sigma222) DO
    Debug.Out(F("sigma222 = %s, weightedSigma = %s, speedSigma = %s",
                DebugPmroSigma(sigma222), LR(weightedSigma), LR(speedSigma)))
  END
END JBay.
