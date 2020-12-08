MODULE Cloudbreak;
FROM SvsTypes IMPORT Corner;

PROCEDURE SetProgram(VAR Ss, Tt, Ff              : Corner;
                     VAR RefP, FixedP, RefLeakP  : LONGREAL;
                     VAR LkgRatio, LkgRatioSigma : LONGREAL;
                     VAR Trunc                   : LONGREAL) =
  BEGIN
    Ss := Corner { 0.710d0, 0.760d0, +3.0d0 };
    Tt := Corner { 0.660d0, 0.710d0,  0.0d0 };
    Ff := Corner { 0.620d0, 0.670d0, -3.0d0 };
    
    (* from the Karthik/Mika/Julianne Excel 2020WW47 *)
    RefP          := 388.6d0;
    FixedP        :=  81.8d0;
    RefLeakP      :=  21.1d0;
    
    
    LkgRatio      :=   2.0d0; (* how much does leakage vary over corner *)
    LkgRatioSigma :=   3.0d0;
    Trunc         := 3.0d0; (* where to truncate the distribution at sort *)
  END SetProgram;

BEGIN END Cloudbreak.
