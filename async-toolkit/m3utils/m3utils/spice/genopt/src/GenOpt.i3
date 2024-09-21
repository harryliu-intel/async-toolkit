INTERFACE GenOpt;
IMPORT SchemeSymbol;
IMPORT OptCallback;
IMPORT LongRealSeq;
IMPORT SchemeObject;
IMPORT Pathname;
IMPORT TextTextTbl;
IMPORT LRVector;

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

PROCEDURE DoIt(optVars, paramVars : SchemeObject.T);

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

  (* sigmaK is for StocRobust minimization ... *)
PROCEDURE SetSigmaK(to : LONGREAL);

PROCEDURE GetSigmaK() : LONGREAL;
  
TYPE
  Method = { NewUOAs, Robust, NewUOA, StocRobust };

PROCEDURE SetMethod(method : Method);

PROCEDURE GetMethod() : Method;

PROCEDURE FmtP(p : LRVector.T) : TEXT;
  
CONST Brand = "GenOpt";

VAR rho  : LONGREAL;
    iter : CARDINAL;
    
END GenOpt.
