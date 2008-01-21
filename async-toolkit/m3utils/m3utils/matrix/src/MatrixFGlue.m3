(* $Id$ *)

UNSAFE MODULE MatrixFGlue EXPORTS Matrix;
FROM MatrixF IMPORT muld_;

PROCEDURE MulD(a,b, prod : T)            RAISES { DimensionMismatch } =
  VAR
    aDim := GetDim(a);
    bDim := GetDim(b);
  BEGIN
    IF GetDim(a).cols # GetDim(b).rows THEN RAISE DimensionMismatch END;

    IF GetDim(prod).rows # GetDim(a).rows OR
      GetDim(prod).cols # GetDim(b).cols THEN
      RAISE DimensionMismatch
    END;

    muld_(ADR(a[0,0]),ADR(b[0,0]),
          ADR(prod[0,0]),
          ADR(aDim.rows),ADR(aDim.cols),ADR(bDim.cols))
  END MulD;



BEGIN END MatrixFGlue.
