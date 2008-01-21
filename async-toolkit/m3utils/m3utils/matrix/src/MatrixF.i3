(* $Id$ *)

UNSAFE INTERFACE MatrixF;
IMPORT Matrix;

<*EXTERNAL*>
PROCEDURE muld_(amatrix, bmatrix, prod, aRows, aCols, bCols : ADDRESS);

END MatrixF.
