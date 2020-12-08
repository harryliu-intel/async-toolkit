MODULE JBay;
FROM SvsTypes IMPORT Corner;

PROCEDURE SetProgram(VAR Ss, Tt, Ff              : Corner;
                     VAR RefP, FixedP, RefLeakP  : LONGREAL;
                     VAR LkgRatio, LkgRatioSigma : LONGREAL;
                     VAR Trunc                   : LONGREAL) =
  CONST
    (* endless discussions 12/3/20 *)

    LabVmin139   =  0.715d0;
    AgeMargin    =  0.020d0;
    Vmin139vsFF  = -10.22d-3;
    VminEolFF    =  LabVmin139 + AgeMargin + Vmin139vsFF;
    Ff2Tt        =  40.0d-3;
    Tt2Ss        =  50.0d-3;

    FfPowV       =  VminEolFF;
    TtPowV       =  FfPowV + Ff2Tt;
    SsPowV       =  TtPowV + Tt2Ss;

    PowToContact = -50.0d-3;
  BEGIN
    
    Ss := Corner { SsPowV + PowToContact, SsPowV, +3.0d0 };
    Tt := Corner { TtPowV + PowToContact, TtPowV,  0.0d0 };
    Ff := Corner { FfPowV + PowToContact, FfPowV, -3.0d0 };
    
    (* from lab measurements : we assume measured chip is at FF *)
    RefP          := 377.90d0;
    FixedP        := 107.0d0;
    RefLeakP      := 26.41d0 / 1.6754d0;
    
    LkgRatio      :=   2.0d0; (* how much does leakage vary over corner *)
    LkgRatioSigma :=   3.0d0;
    Trunc := 3.0d0; (* where to truncate the distribution at sort *)
  END SetProgram;

BEGIN END JBay.
