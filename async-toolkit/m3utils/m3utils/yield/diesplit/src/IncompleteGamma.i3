INTERFACE IncompleteGamma;

(* see Numerical Recipes in Fortran 77, 2nd ed., sec 6-2. *)

PROCEDURE Gammap(a, x : LONGREAL) : LONGREAL;
  (* regularized lower incomplete gamma *)

PROCEDURE Gammaq(a, x : LONGREAL) : LONGREAL;
  (* regularized upper incomplete gamma *)

END IncompleteGamma.
