UNSAFE MODULE M2_F_sp;
IMPORT MatrixF;
IMPORT RMatrix2 AS M2;

PROCEDURE MulMV(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.V) =
  VAR cols := NUMBER(a[0]);
      rows := NUMBER(a);
  BEGIN
    <* ASSERT cols = NUMBER(b) *>
    <* ASSERT rows = NUMBER(prod) *>
    MatrixF.mulmv_sp_(ADR(a[0,0]),ADR(b[0]),ADR(prod[0]),ADR(rows),ADR(cols))
  END MulMV;

PROCEDURE MulTransposeMM(READONLY a,b : M2.M; VAR prod : M2.M) =
  VAR
    aDim := M2.GetDim(a);
    bDim := M2.GetDim(b);
    cDim := M2.GetDim(prod);
  BEGIN
    <* ASSERT aDim.rows = bDim.rows *>
    <* ASSERT bDim.cols = cDim.cols *>
    <* ASSERT aDim.cols = cDim.rows *>
    MatrixF.mul_mtransposem_sp_(ADR(a[0,0]),ADR(b[0,0]),ADR(prod[0,0]),
                                ADR(aDim.rows), ADR(aDim.cols), ADR(bDim.cols))
  END MulTransposeMM;

BEGIN END M2_F_sp.
