MODULE MultiEval EXPORTS MultiEval;

IMPORT MultiEvalClass AS Class;

IMPORT LRVector;
IMPORT LRScalarField;
IMPORT Math;
FROM Fmt IMPORT Int, F, LongReal;
IMPORT Debug;

REVEAL
  T = Class.Private BRANDED Brand OBJECT
  OVERRIDES
    eval     := Eval;
    evalHint := EvalHint;
    init     := Init;
  END;

PROCEDURE Init(t : T; base : LRScalarField.T) : T =
  BEGIN
    t.base := base;
    RETURN t
  END Init;
  
PROCEDURE Eval(t : T; p : LRVector.T) : LONGREAL =
  BEGIN
    RETURN t.base.eval(p)
  END Eval;

PROCEDURE EvalHint(t : T; p : LRVector.T) =
  BEGIN
    t.base.evalHint(p)
  END EvalHint;

PROCEDURE Combine(READONLY a, b : Result) : Result =
  VAR
    nom : LONGREAL;
  BEGIN
    IF a.nominal # 0.0d0 AND b.nominal # 0.0d0 THEN
      IF a.nominal # b.nominal THEN
        Debug.Error(F("a[%s].nominal = %s # b[%s].nominal = %s",
                      Int(a.id),
                      LongReal(a.nominal),
                      Int(b.id),
                      LongReal(b.nominal)))
      END;
      nom := a.nominal
    ELSIF a.nominal # 0.0d0 THEN
      nom := a.nominal
    ELSE
      nom := b.nominal
    END;
    RETURN Result { MAX(a.id, b.id),
                    nom,
                    a.n + b.n,
                    a.sum + b.sum,
                    a.sumsq + b.sumsq }
  END Combine;

PROCEDURE Nominal(READONLY a : Result) : LONGREAL =
  BEGIN
    RETURN a.nominal
  END Nominal;

PROCEDURE Mean(READONLY a : Result) : LONGREAL =
  BEGIN
    RETURN a.sum / FLOAT(a.n, LONGREAL) - a.nominal
  END Mean;

PROCEDURE Var(READONLY a : Result) : LONGREAL =
  BEGIN
    WITH nf     = FLOAT(a.n, LONGREAL),
         mean   = a.sum / nf,
         meansq = a.sumsq / nf DO
      RETURN meansq - mean * mean
    END
  END Var;

PROCEDURE Sdev(READONLY a : Result) : LONGREAL =
  BEGIN
    WITH nf = FLOAT(a.n, LONGREAL) DO
      RETURN Math.sqrt(nf / (nf - 1.0d0) * Var(a))
    END
  END Sdev;


PROCEDURE Format(READONLY a : Result) : TEXT =
  BEGIN
    RETURN F("{ n=%s nom=%s mean=%s sdev=%s }",
             Int(a.n),
             LongReal(Nominal(a)),
             LongReal(Mean(a)),
             LongReal(Sdev(a)))
  END Format;
  
BEGIN END MultiEval.
