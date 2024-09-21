INTERFACE StocRobust;

(* 
   this is the StocRobust stochastic parallel minimizer.
*)

FROM NewUOAs IMPORT Output;
IMPORT LRVector;
IMPORT MultiEval;

PROCEDURE Minimize(p              : LRVector.T;
                   func           : MultiEval.T;
                   rhobeg, rhoend : LONGREAL (* same as Powell *);
                   sigmaK         : LONGREAL) : Output;

CONST Brand = "StocRobust";

PROCEDURE SetSigmaK(k : LONGREAL);
  (* set sigma factor *)

PROCEDURE GetSigmaK() : LONGREAL;
  (* get sigma factor *)

END StocRobust.
