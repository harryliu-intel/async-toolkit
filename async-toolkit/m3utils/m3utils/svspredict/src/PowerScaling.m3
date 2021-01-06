MODULE PowerScaling;
IMPORT TvpMeasurement, Power3;
IMPORT Scale;
IMPORT Debug;
FROM Fmt IMPORT F, LongReal;

CONST LR = LongReal;

PROCEDURE Predict(measAct    : TvpMeasurement.T;
                  measLkg    : TvpMeasurement.T;
                  reqV, reqT : LONGREAL;
                  fixedDynP  : LONGREAL
  ) : Power3.T =
  VAR
    lkgAtAct := measLkg.p *
                Scale.LkgPwrByT(measLkg.t, measAct.t) * 
                Scale.LkgPwrByV(measLkg.v, measAct.v);
    (* leakage power predicted for conditions seen at actual measurement *)

    dynAtAct := measAct.p - lkgAtAct - fixedDynP;
    
    lkgAtReq := measLkg.p *
                Scale.LkgPwrByT(measLkg.t, reqT) * 
                Scale.LkgPwrByV(measLkg.v, reqV);
    
    dynAtReq := dynAtAct *
                Scale.DynPwrByT(measAct.t, reqT) * 
                Scale.DynPwrByV(measAct.v, reqV) + fixedDynP;
  BEGIN
    Debug.Out(F("PredPower measLkg = %s", TvpMeasurement.Fmt(measLkg)));
    Debug.Out(F("PredPower measAct = %s", TvpMeasurement.Fmt(measAct)));
    Debug.Out(F("PredPower reqV = %s, reqT = %s", LR(reqV), LR(reqT)));
    Debug.Out(F("lkgAtAct = %s, dynAtAct = %s, lkgAtReq = %s, dynAtReq = %s",
                LR(lkgAtAct), LR(dynAtAct), LR(lkgAtReq), LR(dynAtReq)));
    
    RETURN Power3.T { dynAtReq, lkgAtReq, dynAtReq + lkgAtReq }
  END Predict;

BEGIN END PowerScaling.
