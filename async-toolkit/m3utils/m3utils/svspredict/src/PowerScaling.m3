(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE PowerScaling;
IMPORT TvpMeasurement, Power3;
IMPORT Scale;
IMPORT Debug;
FROM Fmt IMPORT F, LongReal;
FROM SvsTypes IMPORT CornerData;
IMPORT Power;
IMPORT Corner;

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

PROCEDURE Interpolate1(x : LONGREAL;
                       s, ssigma, t, tsigma, f, fsigma : LONGREAL) : LONGREAL=
  BEGIN
    (* 
       negative -> SLOW 
       positive -> FAST
    *)
    IF       x >=  tsigma THEN
      RETURN x * (f - t) / (fsigma - tsigma) + t
    ELSE  (* x < tsigma *)
      RETURN x * (s - t) / (ssigma - tsigma) + t
    END
  END Interpolate1;
  
PROCEDURE Interpolate(READONLY p : Power.Params;
                               x : LONGREAL) : CornerData =
  VAR
    res : CornerData;
    Ss := p.c[Corner.T.SS];
    Tt := p.c[Corner.T.TT];
    Ff := p.c[Corner.T.FF];
  BEGIN
    res.sigma := x;
    res.vtiming := Interpolate1(x,
                                Ss.vtiming, Ss.sigma,
                                Tt.vtiming, Tt.sigma,
                                Ff.vtiming, Ff.sigma);
    res.vpower  := Interpolate1(x,
                                Ss.vpower, Ss.sigma,
                                Tt.vpower, Tt.sigma,
                                Ff.vpower, Ff.sigma);
    RETURN res
  END Interpolate;
  

BEGIN END PowerScaling.
