(* $Id$ *)

GENERIC INTERFACE ParametricSplineImpl(Spline);
IMPORT ParametricSpline;

CONST Brand = "Parametric" & Spline.Brand;

TYPE T <: ParametricSpline.T;

END ParametricSplineImpl.
