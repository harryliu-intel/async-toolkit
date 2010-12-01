(* $Id$ *)

INTERFACE SXTimer;
IMPORT SXLongReal;

(* similar to facilities of SXTime but uses only a single thread *)

TYPE
  T <: Public; 

  Private <: SXLongReal.T;  

  Public = Private OBJECT METHODS
    init(granularity := 1.0d0) : T;
  END;

CONST Brand = "SXTimer";

END SXTimer.

