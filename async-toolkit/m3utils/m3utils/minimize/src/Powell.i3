(* $Id$ *)

INTERFACE Powell;
IMPORT Matrix, Compress;

PROCEDURE Minimize(VAR p : Matrix.Vector;
                   VAR xi : Matrix.T;
                   ftol : LONGREAL;
                   func : Compress.MultiFunc) : LONGREAL;

END Powell.
