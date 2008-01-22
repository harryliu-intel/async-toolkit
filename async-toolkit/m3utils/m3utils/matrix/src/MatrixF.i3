(* $Id$ *)

UNSAFE INTERFACE MatrixF;

<*EXTERNAL*>
PROCEDURE muld_(amatrix, bmatrix, prod, aRows, aCols, bCols : ADDRESS);

<*EXTERNAL mulmv_sp_*>
PROCEDURE mulmv_sp_(a, v, r, rows, cols : ADDRESS);

<*EXTERNAL lu2_backsubstitute_sp_*>
PROCEDURE lu2_backsubstitute_sp_(m, indx, b, n : ADDRESS);

<*EXTERNAL mul_mtransposem_sp_*>
PROCEDURE mul_mtransposem_sp_(a, b, c, arows, acols, bcols : ADDRESS);

END MatrixF.
