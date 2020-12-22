INTERFACE YieldModel;

PROCEDURE Stapper(A, D0, n, alpha : LONGREAL) : LONGREAL;

PROCEDURE BoseEinstein(A, D0, n : LONGREAL) : LONGREAL;

PROCEDURE Poisson(A, D0, n : LONGREAL) : LONGREAL;

CONST Brand = "YieldModel";

END YieldModel.
