INTERFACE YieldModel;

PROCEDURE Stapper(A, D0, n, alpha : LONGREAL) : LONGREAL;

PROCEDURE BoseEinstein(A, D0, n : LONGREAL) : LONGREAL;

PROCEDURE Poisson(A, D0, n : LONGREAL) : LONGREAL;


  (* helper functions below *)

PROCEDURE StapperGamma(alpha, beta, D : LONGREAL) : LONGREAL;
  (* p.d.f. that Stapper used for his distribution *)

CONST Brand = "YieldModel";

END YieldModel.
