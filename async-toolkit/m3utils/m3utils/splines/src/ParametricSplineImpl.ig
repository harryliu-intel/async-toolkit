(* $Id$ *)

GENERIC INTERFACE ParametricSplineImpl(BaseSpline);
IMPORT Spline;
IMPORT ParametricSpline;

CONST Brand = "Parametric" & BaseSpline.Brand;

TYPE T <: ParametricSpline.T;

END ParametricSplineImpl.
