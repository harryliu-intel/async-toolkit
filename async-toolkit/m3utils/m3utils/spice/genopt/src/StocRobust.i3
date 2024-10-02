INTERFACE StocRobust;

(* 
   this is the StocRobust stochastic parallel minimizer.
*)

FROM NewUOAs IMPORT Output;
IMPORT LRVector;
IMPORT MultiEval;

PROCEDURE Minimize(p              : LRVector.T;
                   func           : MultiEval.T;
                   rhobeg, rhoend : LONGREAL (* same as Powell *)) : Output;

CONST Brand = "StocRobust";

VAR sigmaK := 0.0d0;

PROCEDURE SetSigmaK(k : LONGREAL);
  (* set sigma factor *)

PROCEDURE GetSigmaK() : LONGREAL;
  (* get sigma factor *)

PROCEDURE SetDoNominal(to : BOOLEAN);

PROCEDURE GetDoNominal() : BOOLEAN;

PROCEDURE SetSelectByAll(to : BOOLEAN);
  (* 
     if selectByAll is TRUE, we select the mu/sigma fit by 
     likelihood of all used points.

     if it is FALSE, we select the fit by a validation set of 16 points
  *)
     

PROCEDURE GetSelectByAll() : BOOLEAN;

END StocRobust.
