MODULE JBay;
FROM SvsTypes IMPORT CornerData;
IMPORT N7Tech AS Tech, N7PMRO AS TechPmro;
IMPORT PMRO;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT Power;
IMPORT Corner;

CONST LR = Fmt.LongReal;

TYPE
  TvpMeasurement = RECORD
    t, v, p : LONGREAL;
  END;

PROCEDURE SetProgram(VAR p : Power.Params;
                     VAR Trunc                   : LONGREAL) =
  CONST
    f       = SetProgram139;
  BEGIN
    f(p, Trunc)
  END SetProgram;


PROCEDURE Interpolate1(READONLY spec  : ARRAY OF CARDINAL;
                       READONLY sigma : ARRAY OF LONGREAL;
                       READONLY meas  : CARDINAL) : LONGREAL =
  PROCEDURE Do(i0, i1 : CARDINAL) : LONGREAL =
    BEGIN
      WITH x0 = sigma[i0],
           x1 = sigma[i1],
           y0 = FLOAT(spec [i0], LONGREAL),
           y1 = FLOAT(spec [i1], LONGREAL),
           m  = FLOAT(meas, LONGREAL) DO
        RETURN x0 + (x1 - x0) * (m - y0) / (y1 - y0)
      END
    END Do;
    
    BEGIN
      FOR j := 1 TO LAST(spec) DO
        IF meas <= spec[j] THEN
          RETURN Do(j - 1, j)
        END
      END;
      RETURN Do(LAST(spec) - 1, LAST(spec))
    END Interpolate1;
    
PROCEDURE InterpolatePmro(READONLY specs : ARRAY Tech.Transistor OF PMRO.T;
                          READONLY meas  : ARRAY Tech.Transistor OF CARDINAL) :
  ARRAY Tech.Transistor OF LONGREAL =

  VAR
    res : ARRAY Tech.Transistor OF LONGREAL;
  BEGIN
    FOR i := FIRST(res) TO LAST(res) DO
      res[i] := Interpolate1(specs[i], Tech.CornerSigma, meas[i])
    END;
    RETURN res
  END InterpolatePmro;
  
PROCEDURE SetProgram242(VAR p : Power.Params;
                        VAR Trunc                   : LONGREAL) =
  CONST
    pmro    = ARRAY Tech.Transistor OF CARDINAL { 495, 445, 359 };
    
    MeasLkg = TvpMeasurement { 95.0d0, 0.750d0,  33.188d0 };
    MeasAct = TvpMeasurement { 96.0d0, 0.700d0, 336.0d0   };
    
  VAR
    sigma := InterpolatePmro(TechPmro.V, pmro);
    sumWeight, sumSigma := 0.0d0;
    weightedSigma : LONGREAL;
  BEGIN
    FOR i := FIRST(sigma) TO LAST(sigma) DO
      WITH weight = FinCounts[i] * Tech.TranLeakageRatio[i] DO
        sumWeight := sumWeight + weight;
        sumSigma  := sumSigma  + weight * sigma[i]
      END;
      Debug.Out(F("transistor type %4s sigma %s",
                  Tech.TransistorNames[i], LR(sigma[i])))
    END;

    weightedSigma := sumSigma / sumWeight;
    
    Debug.Out(F("weighted sigma %s",
                LR(weightedSigma)));
    <*ASSERT FALSE*>
  END SetProgram242;
  
PROCEDURE SetProgram139(VAR p : Power.Params;
                        VAR Trunc                   : LONGREAL) =
  CONST
    (* endless discussions 12/3/20 *)

    LabVmin139    =  0.715d0;
    AgeMargin     =  0.020d0;
    Vmin139vsFF   = +10.22d-3;
    VminEolFF     =  LabVmin139 + AgeMargin - Vmin139vsFF;
    Ff2Tt         =  40.0d-3;
    Tt2Ss         =  50.0d-3;

    FfPowV        =  VminEolFF;
    TtPowV        =  FfPowV + Ff2Tt;
    SsPowV        =  TtPowV + Tt2Ss;

    PowToContact  = -50.0d-3;
    ThisLeakSigma = -2.2335d0;
  BEGIN
    p.c := ARRAY Corner.T OF CornerData {
    CornerData { SsPowV + PowToContact, SsPowV, +3.0d0 },
    CornerData { TtPowV + PowToContact, TtPowV,  0.0d0 },
    CornerData { FfPowV + PowToContact, FfPowV, -3.0d0 } };
    
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
    
    p.FixedP        := 107.0d0;
    p.RefLeakP      := 26.41d0 / 1.6754d0;
    
    p.LkgRatio      :=   2.0d0; (* how much does leakage vary over corner *)
    p.LkgRatioSigma :=   3.0d0;
    Trunc         :=   3.0d0; (* where to truncate the distribution at sort *)
  END SetProgram139;

BEGIN END JBay.
