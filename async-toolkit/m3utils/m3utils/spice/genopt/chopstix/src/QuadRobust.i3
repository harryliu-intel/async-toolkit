INTERFACE QuadRobust;

(* 
   this is the QuadRobust stochastic parallel minimizer.
*)

FROM NewUOAs IMPORT Output;
IMPORT LRVector;
IMPORT MultiEvalLRVector;
IMPORT GenOpt;
IMPORT SchemeSymbol;
IMPORT ResponseModel;
IMPORT QuadResponse;
IMPORT ModelVarSeq;

PROCEDURE Minimize(p              : LRVector.T;
                   func           : MultiEvalLRVector.T;
                   rhobeg, rhoend : LONGREAL (* same as Powell *);
                   progressWriter : GenOpt.ResultWriter := NIL) : Output;

CONST Brand = "QuadRobust";

VAR sigmaK := 0.0d0;

PROCEDURE SetDoNominal(to : BOOLEAN);

PROCEDURE GetDoNominal() : BOOLEAN;

PROCEDURE SetSelectByAll(to : BOOLEAN);
  (* 
     if selectByAll is TRUE, we select the mu/sigma fit by 
     likelihood of all used points.

     if it is FALSE, we select the fit by a validation set of 16 points
  *)
     

PROCEDURE GetSelectByAll() : BOOLEAN;

PROCEDURE DoModel(varname : SchemeSymbol.T;
                  models  : ARRAY QuadResponse.T OF ResponseModel.Type);

PROCEDURE OptInit();

PROCEDURE GetOptVars() : ModelVarSeq.T;

END QuadRobust.
