(* $Id$ *)

INTERFACE Compress;
IMPORT Matrix;

(* could be more efficient if we used READONLY *)
TYPE MultiFunc = PROCEDURE (a : Matrix.Vector) : LONGREAL;

(* gradient of MultiFunc *)
(* could be more efficient if we used READONLY *)
TYPE GradMultiFunc = PROCEDURE (a : Matrix.Vector; 
                                VAR gradient : Matrix.Vector);


(* Minimize a multivariate function func along the direction xi starting from
   the point p.

   p will be updated to the minimum along the direction.
   xi will be updated to the amount moved along the direction.
   LinMin returns the minimum value found.
*)

PROCEDURE LinMin(VAR p : Matrix.Vector; (* initial and final point *)
                 VAR xi : Matrix.Vector; (* search direction, 
                                            replaced with change in p *)
                 func : MultiFunc) : LONGREAL (* returns min. value *);

END Compress.
