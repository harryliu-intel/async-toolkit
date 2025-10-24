INTERFACE SurfaceRep;

IMPORT LRVector;
IMPORT LRMatrix2 AS M;
IMPORT Wx;

(* support two ways of modeling a function : linear (L) and quadratic (Q) 

   Each is described using a vector, see ComputeIndepsQ and ComputeIndepsL.

   The size of the vector is the degrees of freedom of the model.

   We support both representations in various ways.
*)

PROCEDURE ComputeIndepsQ(p             : LRVector.T;
                         row           : CARDINAL;
                         VAR(*OUT*) x  : M.M);
  (* fill in x[row,*] with quadratic powers of p as follows:

    p[0]*p[0]
    p[0]*p[1]
    p[0]*p[2]
    ...
    p[0]*p[n-1]
    p[0]
    p[1]*p[1]
    p[1]*p[2]
    ...
    p[1]
    p[2]*p[2]
    ...
    p[n-1]*p[n-1]
    1
  *)
  
PROCEDURE ComputeIndepsL(p             : LRVector.T;
                         row           : CARDINAL;
                         VAR(*OUT*) x  : M.M);
  (* fill in x[row,*] with linear powers of p as follows:
     p[0]
     p[1]
     ...
     p[n-1]
     1
   *)

PROCEDURE ComputeIndepsC(p             : LRVector.T;
                         row           : CARDINAL;
                         VAR(*OUT*) x  : M.M);
  (* fill in x[row,0] with constant term 1 *)

PROCEDURE ComputeQ(p : LRVector.T; b : REF M.M; wx : Wx.T := NIL) : LONGREAL;
  (* compute sum of terms of ComputeIndepsQ ; if wx is given, add some
     debug output to it *)    
    
PROCEDURE ComputeL(p : LRVector.T; b : REF M.M; wx : Wx.T := NIL) : LONGREAL;
  (* compute sum of terms of ComputeIndepsL *)    

PROCEDURE ComputeC(p : LRVector.T; b : REF M.M; wx : Wx.T := NIL) : LONGREAL;
  (* b[0,0] *)

PROCEDURE L2Q(n : CARDINAL; READONLY b : M.M) : REF M.M;
  (* convert linear matrix in format of ComputeIndepsL to 
     quadratic matrix in format of ComputeIndepsQ.
     Fill in coefficients not specified with zero *)

PROCEDURE C2Q(n : CARDINAL; READONLY b : M.M) : REF M.M;
  
PROCEDURE Q2Q(n : CARDINAL; READONLY b : M.M) : REF M.M;

PROCEDURE C2L(n : CARDINAL; READONLY b : M.M) : REF M.M;

PROCEDURE FmtL(n : CARDINAL; b : REF M.M) : TEXT;
  (* print an L matrix for debugging *)
  
PROCEDURE FmtQ(n : CARDINAL; b : REF M.M) : TEXT;
  (* print a Q matrix for debugging *)

PROCEDURE FmtC(n : CARDINAL; b : REF M.M) : TEXT;
  (* print a C matrix (b[0,0]) for debugging *)

PROCEDURE BiggestQuadratic(p : LRVector.T; b : REF M.M) : LONGREAL;
  (* return the largest absolute value of 
     -- all negative squared terms
     -- all cross terms regardless of sign

     ignore linear and constant terms
  *)
  
PROCEDURE ComputeG(p : LRVector.T; b : REF M.M) : LRVector.T;
  (* gradient of the quadratic at point p *)

PROCEDURE GetConstantTerm(b : REF M.M) : LONGREAL;

PROCEDURE Ldofs(n : CARDINAL) : CARDINAL;
  (* number of entries in an L vector supporting an n-dimensional system *)

PROCEDURE Qdofs(n : CARDINAL) : CARDINAL;
  (* number of entries in a Q vector supporting an n-dimensional system *)

PROCEDURE Cdofs(n : CARDINAL) : CARDINAL;
  (* number of entries in a C vector supporting an n-dimensional system (1) *)

TYPE ByOrder = ARRAY [ 0 .. 2 ] OF LONGREAL;
     
PROCEDURE SumAbsCoeffQ(n : CARDINAL; q : REF M.M) : ByOrder;
  (* sum of absolute coefficients by order *)

CONST Brand = "SurfaceRep";

END SurfaceRep.
