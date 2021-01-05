MODULE SvsTypes;
FROM Fmt IMPORT LongReal, F;

CONST LR = LongReal;
      
PROCEDURE FmtCornerData(READONLY cd : CornerData) : TEXT =
  BEGIN
    RETURN F("CornerData{ vtiming=%s vpower=%s sigma=%s }",
             LR(cd.vtiming), LR(cd.vpower), LR(cd.sigma))
  END FmtCornerData;

BEGIN END SvsTypes.
