INTERFACE PointEvaluator;
IMPORT LRVector;
IMPORT LRScalarField;
IMPORT Thread;

CONST Brand = "PointEvaluator";

TYPE
  T <: Public;

  Public = Thread.Closure OBJECT METHODS
    init() : T;
    start(p    : LRVector.T;
          func : LRScalarField.T);
    wait() : LONGREAL;
    quit(); (* will not interrupt running task, can be waited for *)
  END;

PROCEDURE Running() : CARDINAL;
  
END PointEvaluator.
