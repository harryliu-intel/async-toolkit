(* $Id$ *)

INTERFACE ParametricSpline;
IMPORT Spline;

(* two-dimensional parametric cubic splines *)

TYPE
  Coord = Spline.Coord;

CONST Brand = "ParametricSpline";
  
TYPE
  PlotStyle = { X, Y, Parametric };

  (* A ParametricSpline.T is an abstract type *)

  T = OBJECT METHODS
    (* coords are to be fed in order along curve *)
    init(READONLY coords : ARRAY OF Coord) : T;
    
    (* get a point along the spline; you must have 0.0d0 <= param <= 1.0d0 *)
    getParametricPoint(param : LONGREAL) : Coord; 

    (* plot the spline for gnuplot in steps steps *)
    gnuPlotFormat(steps : CARDINAL := 10; 
                  plotStyle := PlotStyle.Parametric) : TEXT;
  END;

END ParametricSpline.
