(* $Id$ *)

INTERFACE Bracket;

TYPE Function = PROCEDURE (a : LONGREAL) : LONGREAL;
TYPE Trio = RECORD a, b, c : LONGREAL END;

(* Starting with Trio { a, b, x }, Initial searches until a bracket is
   found s.t. { a, b, x } bracket the minimum.  Initial returns the
   values of the function at { a, b, x } *)
PROCEDURE Initial(VAR bracket : Trio; func : Function) : Trio;


PROCEDURE Brent(bracket : Trio; f : Function; tol : LONGREAL; 
                VAR xmin : LONGREAL) : LONGREAL;

END Bracket.
