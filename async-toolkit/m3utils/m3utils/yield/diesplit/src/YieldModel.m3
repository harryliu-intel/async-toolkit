MODULE YieldModel;
IMPORT Math;
IMPORT IncompleteGamma;

CONST SqMmPerSqInch = 25.4d0 * 25.4d0;
      
PROCEDURE Stapper(A, D0, n, alpha : LONGREAL) : LONGREAL =
  VAR
    Ai := A / SqMmPerSqInch;
  BEGIN
    RETURN Math.pow(1.0d0 + Ai * D0 / alpha, -n * alpha)
  END Stapper;

PROCEDURE BoseEinstein(A, D0, n : LONGREAL) : LONGREAL =
  VAR
    Ai := A / SqMmPerSqInch;
  BEGIN
    RETURN Math.pow(1.0d0 / (1.0d0 + Ai * D0), n)
  END BoseEinstein;

PROCEDURE Poisson(A, D0, n : LONGREAL) : LONGREAL =
  VAR
    Ai := A / SqMmPerSqInch;
  BEGIN
    RETURN Math.exp(-Ai * D0 * n)
  END Poisson;

PROCEDURE GammaDistPdf(alpha, beta, D : LONGREAL) : LONGREAL =
  BEGIN

    RETURN Math.exp(-Math.gamma(alpha) - alpha * Math.log(beta) + (alpha - 1.0d0) * Math.log(D) - D/beta)

    (*
               
    RETURN 1.0d0 / Math.exp(Math.gamma(alpha)) / Math.pow(beta, alpha) * Math.pow(D, alpha - 1.0d0) * Math.exp(-D / beta)

    *)
  END GammaDistPdf;

PROCEDURE GammaDistCdf(alpha, beta, x : LONGREAL) : LONGREAL =
  BEGIN
    RETURN IncompleteGamma.Gammap(alpha, x / beta)
  END GammaDistCdf;
  
BEGIN END YieldModel.

