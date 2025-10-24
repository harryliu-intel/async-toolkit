(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StatFits;
IMPORT LRMatrix2 AS M;
IMPORT LongrealPQ;
IMPORT LRVector;
IMPORT PointMetricLR;
IMPORT Matrix;
IMPORT MultiEvalLR;
IMPORT ResponseModel;
IMPORT StatComponent;

(* A StatFits.T is a fit of a variable's nom, mu, and sigma 
   on a set of points  

   parr in Attempt() is the input data set, and the output fits 
   are T.bnom, T.bmu, and T.bsigma

   The size of the n-ball in which the fit is performed is different
   for nom vs mu/sigma fits.

   The points used for the nom fit are determined by "nomRho", passed
   in as an argument for Attempt().

   The points used for the mu/sigma fit are determined by a likelihood
   analysis (sum of log likelihoods) such that the likelihood of the
   fit is roughly maximized for the size of the ball.  Generally 
   speaking, a small ball will have a low likelihood, and a larger ball
   will have a larger likelihood simply because there are more 
   points in the larger ball.  But once the ball gets too large, the
   fit will be poor owing to higher-order behaviors in the data set
   than in the model, and the likelihood will again fall.  The ball
   is thus increased in size until the likelihood of the points of the fit
   within the ball reaches a (rough) maximum.
*)

TYPE
  T = RECORD
    debug       : TEXT;
    p0          : LRVector.T;   (* center of fit *)
    i           : CARDINAL;     (* index (of the mu/sigma fit) *)
    n           : CARDINAL;     (* dimensionality of the system *)

    (* 
       likelihood metric applies only to mu and sigma part of fit
       nom is fit according to rho 
    *)
    
    l           : LONGREAL;     (* log likelihood of fit on valid. set *)
    lmu, lsig   : LONGREAL;
    nlf         : LONGREAL;     (* measurements in l *)

    bnom, bmu, bsigma : REF M.M;
                                (* these are expressed as quadratic fits 
                                   -- regardless of order performed
                                *)

    ll          : LONGREAL;     (* log likelihood of fit on full set *)
    nllf        : LONGREAL;     (* measurements in ll *)
    pts         : LongrealPQ.T; (* points keyed by likelihood *)
    evals       : CARDINAL;     (* sum total of evaluations considered *)
    radius      : LONGREAL;     (* actual radius of chosen fit of mu/sigma *)
    
    rank        : ARRAY Ranking OF CARDINAL;

    (* the final fields are the settings that were used to build the model *)
    orders      : ARRAY StatComponent.T OF ResponseModel.Order;
    nomRho      : LONGREAL;
  END;

PROCEDURE Format(READONLY t : T ) : TEXT;

CONST DefaultOrders = ARRAY StatComponent.T OF ResponseModel.Order {
  ResponseModel.Order.Quadratic,
  ResponseModel.Order.Quadratic,
  ResponseModel.Order.Linear
  };
  
PROCEDURE Attempt(p           : LRVector.T;
                  (* point in whose neighborhood to fit *)
                  
                  parr        : REF ARRAY OF PointMetricLR.T;
                  (* input data -- parr[i].result is what is fit by 
                     this array neds to be at least 
                     Ldofs(NUMBER(p^)) + LeaveOut
                     in size
                  *)
                  
                  selectByAll : BOOLEAN;
                  (* if TRUE, select fit by sum of rank of MeanAllL and
                     SumAbsLin; else select by sum of rank of MeanValL and
                     SumAbsLin *)

                  (* orders of fits *)
                  orders := DefaultOrders;

                  nomRho  := 0.0d0;
                  (* how large a region to model for nom; 0.0d0 means
                     do not perform a nom fit at all.
                     (region sizes for mu, sigma are automatic and based
                      on a likelihood calculation) 

                      we must have at least the required number of points
                      within nomRho of p, else we raise NotEnoughPoints
                  *)

                  lambdaMult := 0.0d0;
                  (* ridge regression multiplier *)
                  
  ) : T           (* returns best fit for nom, mu, sigma *)
  RAISES { Matrix.Singular, NotEnoughPoints } ;

EXCEPTION NotEnoughPoints;
          
CONST LeaveOut = 16;
      
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
