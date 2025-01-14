INTERFACE QuadCheckpoint;
IMPORT LRVectorMRVTbl;
IMPORT LRVectorLRTbl;
IMPORT PointResult;

TYPE
  T = OBJECT
    iter     : CARDINAL;
    values   : LRVectorMRVTbl.T;
    fvalues  : LRVectorLRTbl.T;
    pr       : PointResult.T;
    rho      : LONGREAL;
  END;

CONST Brand = "QuadCheckpoint";

END QuadCheckpoint.
