INTERFACE StatFits;
IMPORT LRMatrix2 AS M;
IMPORT LongrealPQ;
IMPORT LRVector;
IMPORT PointMetricLR;
IMPORT Matrix;
IMPORT MultiEvalLR;
IMPORT ResponseModel;

(* A StatFits.T is a fit of a variable's mu and sigma on a set of points  *)

TYPE
  T = RECORD
    debug       : TEXT;
    i           : CARDINAL;     (* index *)
    n           : CARDINAL;
    l           : LONGREAL;     (* log likelihood of fit on valid. set *)
    lmu, lsig   : LONGREAL;
    nlf         : LONGREAL;     (* measurements in l *)
    bmu, bsigma : REF M.M;      (* these are expressed as quadratic fits *)

    ll          : LONGREAL;     (* log likelihood of fit on full set *)
    nllf        : LONGREAL;     (* measurements in ll *)
    pts         : LongrealPQ.T; (* points keyed by likelihood *)
    evals       : CARDINAL;     (* sum total of evaluations considered *)

    rank        : ARRAY Ranking OF CARDINAL;
  END;

PROCEDURE Attempt(p           : LRVector.T;
                  (* point in whose neighborhood to fit *)
                  
                  parr        : REF ARRAY OF PointMetricLR.T;
                  (* input data -- parr[i].result is what is fit by *)
                  
                  selectByAll : BOOLEAN;
                  (* if TRUE, select fit by sum of rank of MeanAllL and
                     SumAbsLin; else select by sum of rank of MeanValL and
                     SumAbsLin *)

                  (* orders of fits *)
                  muOrder := ResponseModel.Order.Quadratic;
                  sgOrder := ResponseModel.Order.Linear;

                  nomRho  := 0.0d0
                  (* how large a region to model for nom 
                     (region sizes for mu, sigma are automatic and based
                      on a likelihood calculation) *)
  
  ) : T           (* returns best fit for nom, mu, sigma *)
  RAISES { Matrix.Singular } ;

TYPE CmpResult = [-1 .. +1];

TYPE CmpProc = PROCEDURE(READONLY a, b : T) : CmpResult;

CONST Compare : CmpProc = NIL;

CONST Brand = "StatFits";

TYPE Ranking = { MeanValL,  (* likelihood on validation points set *)
                 MeanAllL,  (* likelihood on complete points set *)
                 SumAbsLin  (* sum of the absolute values of the linear terms *)
  }; (* see StatFitsCmp for more details *)
      
TYPE (* for sorting by ... whatever ... usually likelihood *)
  PElt = LongrealPQ.Elt OBJECT
    p                    : LRVector.T;
    result               : MultiEvalLR.Result;
    pmu, psig, lmu, lsig : LONGREAL;
  END;
  
END StatFits.
