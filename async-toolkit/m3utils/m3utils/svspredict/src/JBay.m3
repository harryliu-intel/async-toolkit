MODULE JBay;
FROM SvsTypes IMPORT CornerData;

PROCEDURE SetProgram(VAR Ss, Tt, Ff              : CornerData;
                     VAR RefP, FixedP, RefLeakP  : LONGREAL;
                     VAR LkgRatio, LkgRatioSigma : LONGREAL;
                     VAR Trunc                   : LONGREAL) =
  CONST
    f = SetProgram139;
  BEGIN
    f(Ss, Tt, Ff, RefP, FixedP, RefLeakP, LkgRatio, LkgRatioSigma, Trunc)
  END SetProgram;


PROCEDURE SetProgram242(VAR Ss, Tt, Ff              : CornerData;
                        VAR RefP, FixedP, RefLeakP  : LONGREAL;
                        VAR LkgRatio, LkgRatioSigma : LONGREAL;
                        VAR Trunc                   : LONGREAL) =
  CONST
  BEGIN
  END SetProgram242;
  
PROCEDURE SetProgram139(VAR Ss, Tt, Ff              : CornerData;
                        VAR RefP, FixedP, RefLeakP  : LONGREAL;
                        VAR LkgRatio, LkgRatioSigma : LONGREAL;
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
    
    Ss := CornerData { SsPowV + PowToContact, SsPowV, +3.0d0 };
    Tt := CornerData { TtPowV + PowToContact, TtPowV,  0.0d0 };
    Ff := CornerData { FfPowV + PowToContact, FfPowV, -3.0d0 };
    
    (* from lab measurements : we assumed measured chip is at FF *)
    (* These figures are re-computed for a TT chip *)

    RefP          := 377.90d0;

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
    
    FixedP        := 107.0d0;
    RefLeakP      := 26.41d0 / 1.6754d0;
    
    LkgRatio      :=   2.0d0; (* how much does leakage vary over corner *)
    LkgRatioSigma :=   3.0d0;
    Trunc         :=   3.0d0; (* where to truncate the distribution at sort *)
  END SetProgram139;

BEGIN END JBay.
