(* $Id$ *)

INTERFACE Spline;

(* this interface just defines the basic operations on splines *)
(* an implementation can override the methods or add more *)

TYPE
  T = OBJECT METHODS
    eval(at : LONGREAL) : LONGREAL;
  END;

END Spline.
