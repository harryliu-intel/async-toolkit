INTERFACE JBay;
FROM SvsTypes IMPORT Corner;

PROCEDURE SetProgram(VAR Ss, Tt, Ff              : Corner;
                     VAR RefP, FixedP, RefLeakP  : LONGREAL;
                     VAR LkgRatio, LkgRatioSigma : LONGREAL;
                     VAR Trunc                   : LONGREAL);

CONST Brand = "JBay";

END JBay.
