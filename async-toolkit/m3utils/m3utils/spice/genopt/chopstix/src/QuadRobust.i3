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
IMPORT StatComponent;
IMPORT ModelVarSeq;
IMPORT SchemeObject;
IMPORT LRVectorFieldPll;
IMPORT LRVectorLRPairTextTbl;

(* 
   note that there's a pile of static initialization that has to be run
   before calling Minimize, and synchronization of the variables of
   the parameters to Minimize and the setup variables.

   Probably the variable list management should not be done by this module
   but by some more generic module, but let's leave that for later.

   The tricky part is that we also have to override the optvars when we 
   interpolate the functions.
*)

PROCEDURE Minimize(p              : LRVector.T;

                   func           : MultiEvalLRVector.T;
                   (* this is a function for returning the values of the
                      optimization variables, defined by DoModel and 
                      indicated by GetOptVars, in the same sequence as the
                      names returned by GetOptVars, at a given evaluation 
                      point p;
                   
                      we expect func.eval to return a value that includes
                      the Scheme interpreter, with its variables all 
                      properly initialized, for func.eval().extra 
                   *)

                   toEval         : SchemeObject.T;
                   (* this is a function of the optimization variables as
                      well as of the input variables, which is the ultimate
                      quantity that we wish to optimize (to the minimum
                      achievable value) *)

                   rhobeg, rhoend : LONGREAL;
                   (* same as Powell *)

                   recorder       : ResultRecorder;
                   (* after an evaluation, will record the result here *)
                   
                   progressWriter : GenOpt.ResultWriter := NIL;
                   (* write progress *)
                   
                   ) : Output;

CONST Brand = "QuadRobust";

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
                  models  : ARRAY StatComponent.T OF ResponseModel.Order);

PROCEDURE OptInit();

PROCEDURE GetModelVars() : ModelVarSeq.T;

VAR schemeMu : MUTEX;

TYPE
  ResultRecorder = LRVectorFieldPll.T OBJECT
    results            : LRVectorLRPairTextTbl.T;
    resultsMu          : MUTEX;
  END;

END QuadRobust.
