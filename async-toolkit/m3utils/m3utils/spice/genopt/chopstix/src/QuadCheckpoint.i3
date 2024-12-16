INTERFACE QuadCheckpoint;
IMPORT LRVectorMRVTbl;
IMPORT LRVectorLRTbl;

TYPE
  T = OBJECT
    iter     : CARDINAL;
    values   : LRVectorMRVTbl.T;
    fvalues  : LRVectorLRTbl.T;
  END;

CONST Brand = "QuadCheckpoint";

END QuadCheckpoint.
