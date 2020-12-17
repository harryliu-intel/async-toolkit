MODULE Power;

FROM SvsTypes IMPORT CornerData;
IMPORT Math;
IMPORT Corner;

PROCEDURE Calc(READONLY d : Params; at : CornerData) : Result =
  BEGIN
    (* +sigma = slow, less leaky *)
    WITH RefRestPwr     = d.RefP - d.FixedP - d.RefLeakP,
         cornerLkgRatio = Math.pow(d.LkgRatio, -at.sigma / d.LkgRatioSigma),
         ttV            = d.c[Corner.T.TT].vpower,
         voltPwrRatio   = (at.vpower/ttV)*(at.vpower/ttV),
         restPwr        = RefRestPwr * voltPwrRatio,
         leakPwr        = d.RefLeakP * cornerLkgRatio * voltPwrRatio,
         totPwr         = d.FixedP + restPwr + leakPwr DO
      RETURN Result { cornerLkgRatio := cornerLkgRatio,
                      leakPwr        := leakPwr,
                      totPwr         := totPwr }
    END
  END Calc;

BEGIN END Power.
