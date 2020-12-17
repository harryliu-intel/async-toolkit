MODULE Power;

FROM SvsTypes IMPORT CornerData;
IMPORT Math;

PROCEDURE Calc(READONLY d : T; at : CornerData) : Result =
  BEGIN
    (* +sigma = slow, less leaky *)
    WITH RefRestPwr     = d.RefP - d.FixedP - d.RefLeakP,
         cornerLkgRatio = Math.pow(d.LkgRatio, -at.sigma / d.LkgRatioSigma),
         voltPwrRatio   = (at.vpower/d.Tt.vpower)*(at.vpower/d.Tt.vpower),
         restPwr        = RefRestPwr * voltPwrRatio,
         leakPwr        = d.RefLeakP * cornerLkgRatio * voltPwrRatio,
         totPwr         = d.FixedP + restPwr + leakPwr DO
      RETURN Result { cornerLkgRatio := cornerLkgRatio,
                      leakPwr        := leakPwr,
                      totPwr         := totPwr }
    END
  END Calc;

BEGIN END Power.
