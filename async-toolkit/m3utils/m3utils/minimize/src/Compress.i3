(* $Id$ *)

INTERFACE Compress;
IMPORT Matrix;

TYPE MultiFunc = PROCEDURE (a : Matrix.Vector) : LONGREAL;

PROCEDURE LinMin(VAR p : Matrix.Vector; (* initial and final point *)
                 VAR xi : Matrix.Vector; (* search direction, 
                                            replaced with change in p *)
                 func : MultiFunc) : LONGREAL (* returns min. value *);

END Compress.
