(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
INTERFACE LU;
IMPORT Matrix;

PROCEDURE Decompose(m : Matrix.T; 
                    indx : REF ARRAY OF INTEGER; 
                    VAR d : LONGREAL) RAISES { Matrix.Singular };


PROCEDURE BackSubstitute(READONLY m : Matrix.T; 
                           READONLY indx : REF ARRAY OF INTEGER; 
                           b : Matrix.Vector);

END LU.
