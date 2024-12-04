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

PROCEDURE DefOptVar(nm : SchemeSymbol.T; defval, defstep : LONGREAL);

PROCEDURE SetRhoBeg(to : LONGREAL);

PROCEDURE GetRhoBeg() : LONGREAL;

PROCEDURE SetRhoEnd(to : LONGREAL);

PROCEDURE GetRhoEnd() : LONGREAL;

PROCEDURE GetRho() : LONGREAL;

PROCEDURE GetIter() : CARDINAL;

PROCEDURE SetCallback(obj : OptCallback.T);

PROCEDURE GetCoords() : LongRealSeq.T;

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
  p : LongRealSeq.T; (* ugly! *)
  doNetbatch                := TRUE;

    
    
END GenOpt.
