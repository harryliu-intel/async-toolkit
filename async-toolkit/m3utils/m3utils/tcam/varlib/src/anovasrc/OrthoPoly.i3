(* $Id: OrthoPoly.i3,v 1.2 2005/04/22 14:24:25 mika Exp $ *)

INTERFACE OrthoPoly;

(* Orthogonal polynomials *)

PROCEDURE Values(READONLY x, w : ARRAY OF LONGREAL;
                 VAR p : ARRAY OF ARRAY OF LONGREAL;
                 maxOrder := -1);
(* 
   generate the values of the orthogonal polynomials of the 
   0th through maxOrder or NUMBER(p)th order, if maxOrder is -1.

   def'n of polys:

   \Sum w_i p_j(x_i) p_k(x_i) = \delta_{jk}

   normally, w_i > 0, \forall i
*)

END OrthoPoly.
