INTERFACE SurfaceRep;
IMPORT LRVector;
IMPORT LRMatrix2 AS M;
IMPORT Wx;

PROCEDURE ComputeIndepsQ(p      : LRVector.T;
                         row    : CARDINAL;
                         VAR x  : M.M);

  
PROCEDURE ComputeIndepsL(p      : LRVector.T;
                         row    : CARDINAL;
                         VAR x  : M.M);
  
PROCEDURE ComputeQ(p : LRVector.T; b : REF M.M; wx : Wx.T := NIL) : LONGREAL;
    
PROCEDURE ComputeL(p : LRVector.T; b : REF M.M) : LONGREAL;

PROCEDURE L2Q(n : CARDINAL; READONLY b : M.M) : REF M.M;

PROCEDURE FmtL(n : CARDINAL; b : REF M.M) : TEXT;

PROCEDURE FmtQ(n : CARDINAL; b : REF M.M) : TEXT;

PROCEDURE BiggestQuadratic(p : LRVector.T; b : REF M.M) : LONGREAL;

  
PROCEDURE ComputeG(p : LRVector.T; b : REF M.M) : LRVector.T;
  (* gradient of the quadratic *)

PROCEDURE GetConstantTerm(b : REF M.M) : LONGREAL;

PROCEDURE Ldofs(n : CARDINAL) : CARDINAL;

PROCEDURE Qdofs(n : CARDINAL) : CARDINAL;

TYPE ByOrder = ARRAY [ 0 .. 2 ] OF LONGREAL;
     
PROCEDURE SumAbsCoeff(n : CARDINAL; q : REF M.M) : ByOrder;
  (* sum of absolute coefficients by order *)

CONST Brand = "SurfaceRep";

END SurfaceRep.
