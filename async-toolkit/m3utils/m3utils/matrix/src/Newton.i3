(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id: Newton.i3,v 1.1 2000/12/15 08:07:06 mika Exp $ *)
INTERFACE Newton;
IMPORT Quantity,Matrix;

EXCEPTION
  DimensionError;
  NoConvergence(CARDINAL);

(* Multidimensional Newton-Raphson root finder *)

(* does math in-place on vars *)

PROCEDURE Solve(vars : REF ARRAY OF REF LONGREAL; 
                funcs : Quantity.Vector;
                maxsteps : CARDINAL;
                tolerance : LONGREAL) 
  RAISES { DimensionError, NoConvergence, Matrix.Singular , Quantity.Recursive,
           Quantity.IllegalOperands };

END Newton.
