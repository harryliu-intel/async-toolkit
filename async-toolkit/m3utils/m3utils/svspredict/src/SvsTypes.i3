INTERFACE SvsTypes;

TYPE
  CornerData = RECORD
    vtiming, vpower, sigma : LONGREAL;
  END;

CONST Brand = "SvsTypes";

TYPE
  ProgramSetter = PROCEDURE(VAR Ss, Tt, Ff              : CornerData;
                            VAR RefP, FixedP, RefLeakP  : LONGREAL;
                            VAR LkgRatio, LkgRatioSigma : LONGREAL;
                            VAR Trunc                   : LONGREAL);
END SvsTypes.
