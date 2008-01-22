INTERFACE M2_F_sp;
IMPORT RMatrix2 AS M2;

(* single-precision Fortran bindings for Matrix 2 *)

PROCEDURE MulMV(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.V);

PROCEDURE MulTransposeMM(READONLY a,b : M2.M; VAR prod : M2.M);

END M2_F_sp.
