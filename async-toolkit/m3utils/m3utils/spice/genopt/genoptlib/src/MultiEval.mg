GENERIC MODULE MultiEval(Field, Type);

IMPORT LRVector;
FROM Fmt IMPORT Int, F;
IMPORT Debug;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    eval     := Eval;
    evalHint := EvalHint;
    init     := Init;
  END;

PROCEDURE Init(t : T; base : Field.T) : T =
  BEGIN
    t.base := base;
    RETURN t
  END Init;
  
PROCEDURE Eval(t : T; p : LRVector.T) : Type.T =
  BEGIN
    RETURN t.base.eval(p)
  END Eval;

PROCEDURE EvalHint(t : T; p : LRVector.T) =
  BEGIN
    t.base.evalHint(p)
  END EvalHint;

PROCEDURE Combine(READONLY a, b : Result) : Result =
  VAR
    nom : Type.T;
  BEGIN
    IF a.nominal # Type.Null AND b.nominal # Type.Null THEN
      IF a.nominal # b.nominal THEN
        Debug.Error(F("a[%s].nominal = %s # b[%s].nominal = %s",
                      Int(a.id),
                      Type.Format(a.nominal),
                      Int(b.id),
                      Type.Format(b.nominal)))
      END;
      nom := a.nominal
    ELSIF a.nominal # Type.Null THEN
      nom := a.nominal
    ELSE
      nom := b.nominal
    END;
    RETURN Result { MAX(a.id, b.id),
                    nom,
                    a.n + b.n,
                    Type.Plus(a.sum, b.sum),
                    Type.Plus(a.sumsq, b.sumsq),
                    NIL }
  END Combine;

PROCEDURE Nominal(READONLY a : Result) : Type.T =
  BEGIN
    RETURN a.nominal
  END Nominal;

PROCEDURE Mean(READONLY a : Result) : Type.T =
  BEGIN
    WITH factor = 1.0d0 / FLOAT(a.n, LONGREAL) DO
      RETURN Type.Minus(Type.ScalarMul(factor, a.sum), a.nominal)
    END
  END Mean;

PROCEDURE Var(READONLY a : Result) : Type.T =
  BEGIN
    WITH fact   = 1.0d0 / FLOAT(a.n, LONGREAL),
         mean   = Type.ScalarMul(fact, a.sum),
         meansq = Type.ScalarMul(fact, a.sumsq) DO
      RETURN Type.Minus(meansq, Type.Times(mean, mean))
    END
  END Var;

PROCEDURE Sdev(READONLY a : Result) : Type.T =
  BEGIN
    IF a.n = 1 THEN
      (* just set the sdev to be 2x the mean *)
      RETURN Type.ScalarMul(2.0d0, Type.Plus(Type.Abs(Nominal(a)),
                                             Type.Abs(Mean(a))))
    END;
    
    WITH nf = FLOAT(a.n, LONGREAL) DO
      RETURN Type.Sqrt(Type.ScalarMul(nf / (nf - 1.0d0), Var(a)))
    END
  END Sdev;


PROCEDURE Format(READONLY a : Result) : TEXT =
  BEGIN
    RETURN F("{ n=%s nom=%s mean=%s sdev=%s }",
             Int(a.n),
             Type.Format(Nominal(a)),
             Type.Format(Mean(a)),
             Type.Format(Sdev(a)))
  END Format;
  
BEGIN END MultiEval.
