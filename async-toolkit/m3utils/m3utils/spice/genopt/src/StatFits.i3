INTERFACE StatFits;
IMPORT LRMatrix2 AS M;
IMPORT LongrealPQ;
IMPORT LRVector;
IMPORT MultiEval;
IMPORT PointMetric;
IMPORT Matrix;

TYPE
  T = RECORD
    debug       : TEXT;
    i           : CARDINAL;     (* index *)
    n           : CARDINAL;
    l           : LONGREAL;     (* log likelihood of fit on valid. set *)
    lmu, lsig   : LONGREAL;
    nlf         : LONGREAL;     (* measurements in l *)
    bmu, bsigma : REF M.M;      (* these are quadratic fits *)

    ll          : LONGREAL;     (* log likelihood of fit on full set *)
    nllf        : LONGREAL;     (* measurements in ll *)
    pts         : LongrealPQ.T; (* points keyed by likelihood *)
    evals       : CARDINAL;     (* sum total of evaluations considered *)

    rank        : ARRAY Ranking OF CARDINAL;
  END;
  
TYPE
  PElt = LongrealPQ.Elt OBJECT
    p                    : LRVector.T;
    result               : MultiEval.Result;
    pmu, psig, lmu, lsig : LONGREAL;
  END;
  
PROCEDURE Attempt(p           : LRVector.T;
                  parr        : REF ARRAY OF PointMetric.T;
                  selectByAll : BOOLEAN ) : T
  RAISES { Matrix.Singular } ;
  
PROCEDURE Attempt1(parr : REF ARRAY OF PointMetric.T)
  RAISES { Matrix.Singular } ;

CONST Brand = "StatFits";

TYPE CmpResult = [-1 .. +1];
      
PROCEDURE CompareByMeanValLikelihood(READONLY a, b : T) : CmpResult;
  (* HIGHER likelihood compares lower *)

PROCEDURE CompareByMeanAllLikelihood(READONLY a, b : T) : CmpResult;
  (* HIGHER likelihood compares lower *)

PROCEDURE CompareBySumAbsLinCoeff(READONLY a, b : T) : CmpResult;
  (* LOWER coefficient compares lower *)

CONST Compare = CompareByMeanValLikelihood;
 
TYPE CmpProc = PROCEDURE(READONLY a, b : T) : CmpResult;

TYPE Ranking = { MeanValL, MeanAllL, SumAbsLin };
      
END StatFits.
