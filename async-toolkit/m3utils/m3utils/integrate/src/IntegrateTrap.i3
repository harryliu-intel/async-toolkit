(* $Id$ *)

INTERFACE IntegrateTrap;

(* integrate things using the trapezoidal rule *)

EXCEPTION NoConvergence;

TYPE Func = PROCEDURE (x : LONGREAL) : LONGREAL;

(* integrate func from a to b in 2^n+1 steps using the trapezoid rule *)
PROCEDURE IntegrateN(func : Func; 
                     a , b : LONGREAL;
                     n : CARDINAL) : LONGREAL;


CONST DefaultEps = 1.0d-5;

(* integrate func from a to b until the fractional accuracy eps *)
(* has been reached; abort if more than 2^jmax steps have been taken. *)
PROCEDURE IntegrateE(func : Func; 
                     a , b : LONGREAL;
                     eps := DefaultEps;
                     jmax : CARDINAL  := 20) : LONGREAL RAISES { NoConvergence };

PROCEDURE SimpsonE(func : Func; 
                   a , b : LONGREAL;
                   eps := DefaultEps;
                   jmax : CARDINAL := 20) : LONGREAL RAISES { NoConvergence };

END IntegrateTrap.
