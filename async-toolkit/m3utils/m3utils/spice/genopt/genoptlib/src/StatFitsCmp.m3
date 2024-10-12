MODULE StatFitsCmp EXPORTS StatFits;
IMPORT LongrealType;
IMPORT SurfaceRep;

PROCEDURE CompareByMeanValLikelihood(READONLY a, b : T) : CmpResult =
  BEGIN
    WITH av = a.l / a.nlf, bv = b.l / b.nlf DO
      RETURN -LongrealType.Compare(av, bv)
    END
  END CompareByMeanValLikelihood;

PROCEDURE CompareByMeanAllLikelihood(READONLY a, b : T) : CmpResult =
  BEGIN
    WITH av = a.ll / a.nllf, bv = b.ll / b.nllf DO
      RETURN -LongrealType.Compare(av, bv)
    END
  END CompareByMeanAllLikelihood;
  
PROCEDURE CompareBySumAbsLinCoeff(READONLY a, b : T) : CmpResult =
  BEGIN
    WITH amuc    = SurfaceRep.SumAbsCoeff(a.n, a.bmu),
         asigmac = SurfaceRep.SumAbsCoeff(a.n, a.bsigma),
         bmuc    = SurfaceRep.SumAbsCoeff(b.n, b.bmu),
         bsigmac = SurfaceRep.SumAbsCoeff(b.n, b.bsigma) DO
      RETURN LongrealType.Compare(amuc[1] + asigmac[1],
                                  bmuc[1] + bsigmac[1])
    END
  END CompareBySumAbsLinCoeff;
  
BEGIN END StatFitsCmp.
