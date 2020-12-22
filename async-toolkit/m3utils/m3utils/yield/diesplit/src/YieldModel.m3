MODULE YieldModel;
IMPORT Math;

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

BEGIN END YieldModel.

