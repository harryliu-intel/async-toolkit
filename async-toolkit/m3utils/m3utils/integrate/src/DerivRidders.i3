(* $Id$ *)

INTERFACE DerivRidders;

(* take derivative of func using Ridders's method *)

TYPE Func = PROCEDURE (x : LONGREAL) : LONGREAL;

PROCEDURE Deriv(func : Func; 
                x : LONGREAL;      (* point at which deriv is taken *)
                h : LONGREAL;      (* "big" step in x; must be nonzero *)
                VAR err : LONGREAL (* estimate of error *)
               ) : LONGREAL;

END DerivRidders.
