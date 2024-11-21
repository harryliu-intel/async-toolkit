INTERFACE StatFitsCmp;
FROM StatFits IMPORT T, CmpResult;
      
PROCEDURE CompareByMeanValLikelihood(READONLY a, b : T) : CmpResult;
  (* HIGHER likelihood compares lower *)

PROCEDURE CompareByMeanAllLikelihood(READONLY a, b : T) : CmpResult;
  (* HIGHER likelihood compares lower *)

PROCEDURE CompareBySumAbsLinCoeff(READONLY a, b : T) : CmpResult;
  (* LOWER coefficient compares lower *)

END StatFitsCmp.
