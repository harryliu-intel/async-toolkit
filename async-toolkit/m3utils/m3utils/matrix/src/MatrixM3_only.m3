(* $Id$ *)

MODULE MatrixM3_only EXPORTS Matrix;
IMPORT MatrixM3;

PROCEDURE MulD(a, b, prod : T) RAISES { DimensionMismatch } =
  BEGIN MatrixM3.MulD(a,b,prod) END MulD;

BEGIN END MatrixM3_only.
