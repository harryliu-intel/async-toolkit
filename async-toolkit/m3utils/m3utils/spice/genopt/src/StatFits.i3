INTERFACE StatFits;
IMPORT LRMatrix2 AS M;
IMPORT LongrealPQ;
IMPORT LRVector;
IMPORT MultiEval;
IMPORT PointMetric;

TYPE
  T = RECORD
    l           : LONGREAL;     (* log likelihood of fit on valid. set *)
    bmu, bsigma : REF M.M;      (* these are quadratic fits *)

    ll          : LONGREAL;     (* log likelihood of fit on full set *)
    pts         : LongrealPQ.T; (* points keyed by likelihood *)
    evals       : CARDINAL;     (* sum total of evaluations considered *)
  END;
  
TYPE
  PElt = LongrealPQ.Elt OBJECT
    p : LRVector.T;
    result : MultiEval.Result;
    pmu, psig, lmu, lsig : LONGREAL;
  END;
  
PROCEDURE Attempt(p           : LRVector.T;
                  parr        : REF ARRAY OF PointMetric.T;
                  selectByAll : BOOLEAN ) : T;
  
PROCEDURE Attempt1(parr : REF ARRAY OF PointMetric.T);

END StatFits.
