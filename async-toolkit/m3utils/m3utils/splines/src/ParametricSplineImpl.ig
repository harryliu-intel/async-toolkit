(* $Id$ *)

GENERIC INTERFACE ParametricSplineImpl(BaseSpline);
IMPORT ParametricSpline;

CONST Brand = "Parametric" & BaseSpline.Brand;

TYPE T <: ParametricSpline.T;

END ParametricSplineImpl.
