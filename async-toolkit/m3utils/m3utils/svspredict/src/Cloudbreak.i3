INTERFACE Cloudbreak;
FROM SvsTypes IMPORT CornerData;

PROCEDURE SetProgram(VAR Ss, Tt, Ff              : CornerData;
                     VAR RefP, FixedP, RefLeakP  : LONGREAL;
                     VAR LkgRatio, LkgRatioSigma : LONGREAL;
                     VAR Trunc                   : LONGREAL);

CONST Brand = "Cloudbreak";

END Cloudbreak.
