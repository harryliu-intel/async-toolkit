INTERFACE GenOpt;

IMPORT SchemeSymbol;
IMPORT OptCallback;
IMPORT LongRealSeq;
IMPORT SchemeObject;
IMPORT Pathname;
IMPORT TextTextTbl;
IMPORT Wr;
IMPORT OSError;
IMPORT NewUOAs;
IMPORT TextSeq;
IMPORT OptVarSeq;
IMPORT LRVector;

PROCEDURE DefOptVar(nm : SchemeSymbol.T; defval, defstep : LONGREAL);

PROCEDURE SetRhoBeg(to : LONGREAL);

PROCEDURE GetRhoBeg() : LONGREAL;

PROCEDURE SetRhoEnd(to : LONGREAL);

PROCEDURE GetRhoEnd() : LONGREAL;

PROCEDURE GetRho() : LONGREAL;

PROCEDURE GetIter() : CARDINAL;

PROCEDURE SetCallback(obj : OptCallback.T);

(**********************************************************************)

(* 
   in the following, if override is NIL, we use the global/static 

   p 

   if we want to override the p for a particular interpreter,
   allocate a new one using NewCoords(), and pass it in

   The idea is that a default NIL setting can be used, and if we want
   to override it for a given interpreter, simply set! the default
   to a NewCoords() and set it as desired.
*)

PROCEDURE GetCoords(override : LongRealSeq.T := NIL) : LongRealSeq.T;

PROCEDURE SetCoord(i        : CARDINAL;
                   to       : LONGREAL;
                   override : LongRealSeq.T := NIL);
  
PROCEDURE SetCoords(to : LRVector.T; override : LongRealSeq.T := NIL);

PROCEDURE NewCoords() : LongRealSeq.T;
  
(**********************************************************************)

PROCEDURE OptInit();

VAR DoIt : PROCEDURE();

PROCEDURE SetNetbatch(to : BOOLEAN);

PROCEDURE DefSchemaPath(path : Pathname.T);

PROCEDURE DefLoadScm(scmPath : Pathname.T);

PROCEDURE DefDataFilename(fnm : Pathname.T);

PROCEDURE DefEval(obj : SchemeObject.T);

PROCEDURE GetParamBindings() : TextTextTbl.T;

PROCEDURE GetParam(named : SchemeSymbol.T) : SchemeObject.T;

PROCEDURE SetOptFailureIsError();

PROCEDURE SetOptFailureResult(res : LONGREAL);

PROCEDURE GetOptFailureResult() : LONGREAL;

TYPE
  Method = { NewUOAs, Robust, NewUOA, StocRobust };

PROCEDURE SetMethod(method : Method);

PROCEDURE GetMethod() : Method;

CONST Brand = "GenOpt";

VAR rho  : LONGREAL;
    iter : CARDINAL;
    paramBindings : TextTextTbl.T;
    
TYPE
  ResultWriter = OBJECT
  METHODS
    write(output : NewUOAs.Output) RAISES { Wr.Failure, OSError.E };
  END;

VAR
  vseq           : OptVarSeq.T;
  rhoBeg, rhoEnd : LONGREAL;
  scmCb          : OptCallback.T;
  schemaPath     : Pathname.T;
  schemaScmPaths : TextSeq.T;
  schemaDataFn   : Pathname.T;
  schemaEval     : SchemeObject.T;
  outOfDomainResult        := FIRST(LONGREAL);
  method                   := Method.NewUOAs;
  doNetbatch                := TRUE;

END GenOpt.
