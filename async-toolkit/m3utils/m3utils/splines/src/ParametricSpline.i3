(* $Id$ *)

INTERFACE ParametricSpline;
IMPORT Spline;

TYPE
  Coord = Spline.Coord;

CONST Brand = "ParametricSpline";
  
TYPE
  T <: Public;

  Public = OBJECT METHODS
    (* coords are in order along curve *)
    init(READONLY coords : ARRAY OF Coord) : T;
    getParametricPoint(param : LONGREAL) : Coord; (* 0.0d0 <= param <= 1.0d0 *)
    gnuPlotFormat(steps : CARDINAL := 10) : TEXT;
  END;

END ParametricSpline.
