INTERFACE SvsTypes;

TYPE
  Corner = RECORD
    vtiming, vpower, sigma : LONGREAL;
  END;

CONST Brand = "SvsTypes";

TYPE
  ProgramSetter = PROCEDURE(VAR Ss, Tt, Ff              : Corner;
                            VAR RefP, FixedP, RefLeakP  : LONGREAL;
                            VAR LkgRatio, LkgRatioSigma : LONGREAL;
                            VAR Trunc                   : LONGREAL);
END SvsTypes.
