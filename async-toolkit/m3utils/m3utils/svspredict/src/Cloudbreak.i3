INTERFACE Cloudbreak;
FROM SvsTypes IMPORT Corner;

PROCEDURE SetProgram(VAR Ss, Tt, Ff              : Corner;
                     VAR RefP, FixedP, RefLeakP  : LONGREAL;
                     VAR LkgRatio, LkgRatioSigma : LONGREAL;
                     VAR Trunc                   : LONGREAL);

CONST Brand = "Cloudbreak";

END Cloudbreak.
