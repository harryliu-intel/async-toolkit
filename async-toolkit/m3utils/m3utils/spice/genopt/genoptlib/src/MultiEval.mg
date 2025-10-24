(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC MODULE MultiEval(Field, Type, TypeSeq);

IMPORT LRVector;
FROM Fmt IMPORT Int, F, LongReal, FN;
IMPORT Debug;
IMPORT Text;
IMPORT Pathname;
IMPORT MapError;

TYPE TA = ARRAY OF TEXT;
     
CONST LR = LongReal;

VAR doDebug := Debug.DebugThis("MultiEval");
    
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
  
PROCEDURE Eval(t : T; p : LRVector.T) : Type.T RAISES { MapError.E } =
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
                    a.extra,
                    PathnameMax(a.subdirPath, b.subdirPath),
                    TypeSeq.Cat(a.seq, b.seq)
                    }
  END Combine;

PROCEDURE PathnameMax(a, b : Pathname.T) : Pathname.T =
  BEGIN
    IF    a = NIL THEN
      RETURN b
    ELSIF b = NIL THEN
      RETURN a
    ELSIF Text.Compare(a, b) = 1 THEN
      RETURN a
    ELSE
      RETURN b
    END
  END PathnameMax;

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
    <*ASSERT a.n # 0*>
    WITH fact   = 1.0d0 / FLOAT(a.n, LONGREAL),
         mean   = Type.ScalarMul(fact, a.sum),
         meansq = Type.ScalarMul(fact, a.sumsq),
         var    = Type.Minus(meansq, Type.Times(mean, mean)),
         tvar   = Type.ZeroLT(0.0d0, var) DO

      IF doDebug THEN
        Debug.Out(FN("MultiEval(%s) : fact %s ; mean %s ; meansq %s : var %s ; tvar %s",
                     TA { Brand, LR(fact), Type.Format(mean), Type.Format(meansq),
                          Type.Format(var), Type.Format(tvar) } ))
      END;
      
      RETURN tvar
    END
  END Var;

PROCEDURE Sdev(READONLY a : Result) : Type.T =
  BEGIN
    IF a.n = 1 THEN
      (* just set the sdev to be 2x the mean *)
      WITH res = Type.ScalarMul(2.0d0, Type.Plus(Type.Abs(Nominal(a)),
                                                 Type.Abs(Mean(a)))) DO
        IF doDebug THEN
          Debug.Out(F("Sdev : 2x path : res %s", Type.Format(res)))
        END;
        RETURN res
      END
    END;
    
    WITH nf  = FLOAT(a.n, LONGREAL),
         res = Type.Sqrt(Type.ScalarMul(nf / (nf - 1.0d0), Var(a))) DO
      IF doDebug THEN
        Debug.Out(F("Sdev : normal path : res %s", Type.Format(res)))
      END;
      RETURN res
    END
  END Sdev;


PROCEDURE Format(READONLY a : Result) : TEXT =
  VAR
    nomStr : TEXT;
  BEGIN
    IF a.nominal = Type.Null THEN
      nomStr := "*NIL*"
    ELSE
      nomStr := Type.Format(Nominal(a))
    END;
    
    RETURN F("{ n=%s nom=%s mean=%s sdev=%s }",
             Int(a.n),
             nomStr,
             Type.Format(Mean(a)),
             Type.Format(Sdev(a)))
  END Format;
  
BEGIN END MultiEval.
