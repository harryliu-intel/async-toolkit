(* $Id$ *)

INTERFACE ConjGradient;
IMPORT Matrix, Compress;

EXCEPTION TooManyIterations;

PROCEDURE Minimize(VAR p : Matrix.Vector;
                   ftol : LONGREAL;
                   func : Compress.MultiFunc;
                   dfunc : Compress.GradMultiFunc) : LONGREAL RAISES { TooManyIterations };

END ConjGradient.
