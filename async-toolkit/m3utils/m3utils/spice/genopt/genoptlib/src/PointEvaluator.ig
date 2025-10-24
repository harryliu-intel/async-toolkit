GENERIC INTERFACE PointEvaluator(Field);
IMPORT LRVector;
IMPORT Thread;

(* evaluate a function (scalar or vector valued), in the background, 
   at a specific point *)

TYPE
  T <: Public;

  Public = Thread.Closure OBJECT METHODS
    init() : T;
    start(p    : LRVector.T;
          func : Field.T);
    wait() : Field.Result;
    quit(); (* will not interrupt running task, can be waited for *)
  END;

PROCEDURE Running() : CARDINAL;

CONST Brand = "PointEvaluator(" & Field.Brand & ")";
  
END PointEvaluator.
