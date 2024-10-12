INTERFACE PointResult;
IMPORT LRVector;

TYPE
  T = RECORD
    p         : LRVector.T;
    metric    : LONGREAL;
    quadratic : BOOLEAN;
    rho       : LONGREAL;
  END;

CONST Brand = "PointResult";

PROCEDURE Format(READONLY a : T) : TEXT;

END PointResult.
    
