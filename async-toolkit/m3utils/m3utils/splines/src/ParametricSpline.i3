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
  END;

END ParametricSpline.
