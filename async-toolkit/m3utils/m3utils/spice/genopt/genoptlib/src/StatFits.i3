INTERFACE StatFits;
IMPORT LRMatrix2 AS M;
IMPORT LongrealPQ;
IMPORT LRVector;
IMPORT MultiEvalLR;
IMPORT PointMetricLR AS PointMetric;
IMPORT Matrix;

(* A StatFits.T is a fit of a variable's mu and sigma on a set of points  *)

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
    result               : MultiEvalLR.Result;
    pmu, psig, lmu, lsig : LONGREAL;
  END;
  
PROCEDURE Attempt(p           : LRVector.T;
                  parr        : REF ARRAY OF PointMetric.T;
                  selectByAll : BOOLEAN ) : T
  RAISES { Matrix.Singular } ;
  
PROCEDURE Attempt1(parr : REF ARRAY OF PointMetric.T)
  RAISES { Matrix.Singular } ;

TYPE CmpResult = [-1 .. +1];

TYPE CmpProc = PROCEDURE(READONLY a, b : T) : CmpResult;

CONST Compare : CmpProc = NIL;

CONST Brand = "StatFits";

TYPE Ranking = { MeanValL,  (* likelihood on validation points set *)
                 MeanAllL,  (* likelihood on complete points set *)
                 SumAbsLin  (* sum of the absolute values of the linear terms *)
  }; (* see StatFitsCmp for more details *)
      
END StatFits.
