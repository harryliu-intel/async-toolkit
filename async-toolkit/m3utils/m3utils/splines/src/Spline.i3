(* $Id$ *)

INTERFACE Spline;

(* this interface just defines the basic operations on splines *)
(* an implementation can override the methods or add more *)

TYPE
  Coord = RECORD
    x, y : LONGREAL
  END;

  T <: Public;

  Public = OBJECT METHODS
    eval(at : LONGREAL) : LONGREAL;
    deriv(at : LONGREAL) : LONGREAL; (* take derivative; uses Ridders 
                                        algorithm by default;
                                        implementors are urged to 
                                        provide analytic derivatives if
                                        possible *)
  END;

CONST Brand = "Spline";

END Spline.
