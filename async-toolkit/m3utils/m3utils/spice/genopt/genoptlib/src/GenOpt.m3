MODULE GenOpt;

IMPORT TextTextTbl;
IMPORT SchemeSymbol;
IMPORT SchemeObject;
IMPORT Pathname;
IMPORT LongRealSeq AS LRSeq;
IMPORT OptCallback;
IMPORT TextSeq;
IMPORT OptVar;
IMPORT OptVarSeq;
IMPORT SchemeString;
IMPORT SchemeBoolean;
IMPORT LRVector;

PROCEDURE GetParamBindings() : TextTextTbl.T =
  BEGIN RETURN paramBindings END GetParamBindings;

PROCEDURE GetParam(named : SchemeSymbol.T) : SchemeObject.T =
  (* returns #f if not defined; returns string if defined *)
  VAR
    val : TEXT;
  BEGIN
    WITH tnm    = SchemeSymbol.ToText(named),
         haveIt = paramBindings.get(tnm, val) DO
      IF haveIt THEN
        RETURN SchemeString.FromText(val)
      ELSE
        RETURN SchemeBoolean.False()
      END
    END
  END GetParam;

PROCEDURE SetMethod(m : Method) = BEGIN method := m END SetMethod;

PROCEDURE GetMethod() : Method = BEGIN RETURN method END GetMethod;

  PROCEDURE SetOptFailureIsError() =
  BEGIN
    outOfDomainResult := FIRST(LONGREAL)
  END SetOptFailureIsError;

PROCEDURE SetOptFailureResult(res : LONGREAL) =
  BEGIN
    outOfDomainResult := res
  END SetOptFailureResult;

PROCEDURE GetOptFailureResult() : LONGREAL =
  BEGIN
    RETURN outOfDomainResult
  END GetOptFailureResult;
  
PROCEDURE OptInit() =
  BEGIN
    vseq   := NEW(OptVarSeq.T).init();
    rhoBeg := FIRST(LONGREAL);
    rhoEnd := LAST(LONGREAL);
    p      := NEW(LRSeq.T).init();
    schemaPath := NIL;
    schemaScmPaths := NEW(TextSeq.T).init();
    schemaDataFn := NIL;
    schemaEval := NIL;
  END OptInit;

PROCEDURE DefSchemaPath(path : Pathname.T) =
  BEGIN schemaPath := path END DefSchemaPath;

PROCEDURE DefLoadScm(scmPath : Pathname.T) =
  BEGIN
    schemaScmPaths.addhi(scmPath)
  END DefLoadScm;

PROCEDURE DefDataFilename(fnm : Pathname.T) =
  BEGIN schemaDataFn := fnm END DefDataFilename;

PROCEDURE DefEval(obj : SchemeObject.T) =
  BEGIN schemaEval := obj END DefEval;
  
PROCEDURE DefOptVar(nm : SchemeSymbol.T; defval, defstep : LONGREAL) =
  BEGIN
    vseq.addlo(OptVar.T { SchemeSymbol.ToText(nm), defval, defstep });
    p.addlo(defval / defstep)
  END DefOptVar;

PROCEDURE SetRhoBeg(to : LONGREAL) =
  BEGIN
    rhoBeg := to
  END SetRhoBeg;

PROCEDURE GetRhoBeg() : LONGREAL =
  BEGIN
    RETURN rhoBeg
  END GetRhoBeg;

PROCEDURE SetRhoEnd(to : LONGREAL) =
  BEGIN
    rhoEnd := to
  END SetRhoEnd;

PROCEDURE GetRhoEnd() : LONGREAL =
  BEGIN
    RETURN rhoEnd
  END GetRhoEnd;

PROCEDURE GetRho() : LONGREAL =
  BEGIN
    RETURN rho
  END GetRho;

PROCEDURE GetIter() : CARDINAL =
  BEGIN
    RETURN iter
  END GetIter;

VAR p : LRSeq.T; (* ugly! *)

PROCEDURE SetCoord(i : CARDINAL; to : LONGREAL; override : LRSeq.T) =
  BEGIN
    IF override # NIL THEN
      override.put(i, to)
    ELSE
      p.put(i, to)
    END
  END SetCoord;

PROCEDURE SetCoords(to : LRVector.T; override : LRSeq.T) =
  BEGIN
    FOR i := FIRST(to^) TO LAST(to^) DO
      SetCoord(i, to[i], override)
    END
  END SetCoords;
  
PROCEDURE GetCoords(override : LRSeq.T) : LRSeq.T =
  BEGIN
    IF override # NIL THEN
      RETURN override
    ELSE
      RETURN p
    END
  END GetCoords;

PROCEDURE NewCoords() : LRSeq.T =
  BEGIN
    WITH new = NEW(LRSeq.T).init() DO
      FOR i := 0 TO p.size() - 1 DO
        new.addhi(p.get(i))
      END;
      RETURN new
    END
  END NewCoords;

PROCEDURE SetCallback(obj : OptCallback.T) =
  BEGIN
    scmCb := obj
  END SetCallback;

PROCEDURE SetNetbatch(to : BOOLEAN) =
  BEGIN
    doNetbatch := to
  END SetNetbatch;

BEGIN
  paramBindings := NEW(TextTextTbl.Default).init();

END GenOpt.
