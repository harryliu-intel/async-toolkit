(* $Id$ *)

INTERFACE ConjGradient;
IMPORT Matrix, Compress;

PROCEDURE Minimize(VAR p : Matrix.Vector;
                   ftol : LONGREAL;
                   func : Compress.MultiFunc;
                   dfunc : Compress.GradMultiFunc) : LONGREAL;

END ConjGradient.
