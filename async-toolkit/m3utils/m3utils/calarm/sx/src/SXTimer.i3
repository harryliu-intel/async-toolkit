(* $Id$ *)

INTERFACE SXTimer;
IMPORT SXLongReal;

(* similar to facilities of SXTime but uses only a single thread *)

TYPE
  T <: Public; 
  
  Public = SXLongReal.T OBJECT METHODS
    init(granularity := 1.0d0) : T;
  END;

CONST Brand = "SXTimer";

END SXTimer.

