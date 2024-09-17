INTERFACE LineMinimizer;
IMPORT LRVector;
IMPORT LRScalarField;
IMPORT LineProblem;
IMPORT Thread;

CONST Brand = "LineMinimizer";

TYPE
  T <: Public;

  Public = Thread.Closure OBJECT METHODS
    init() : T;
    start(pp   : LRVector.T;
          dir  : LRVector.T;
          func : LRScalarField.T;
          rho  : LONGREAL);
    wait() : LineProblem.T;
    quit(); (* will not interrupt running task, can be waited for *)
  END;

PROCEDURE Running() : CARDINAL;
  
END LineMinimizer.
