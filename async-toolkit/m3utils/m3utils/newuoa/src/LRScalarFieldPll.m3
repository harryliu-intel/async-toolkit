MODULE LRScalarFieldPll;
IMPORT LRScalarField;
IMPORT LRVectorLRTbl;
IMPORT LRVectorRefTbl;
IMPORT LRVector;
IMPORT Thread;

REVEAL
  T = Public BRANDED Brand OBJECT
    base : LRScalarField.T;

    mu   : MUTEX;
    tbl  : LRVectorLRTbl.T;
    open : LRVectorRefTbl.T;
    
  OVERRIDES
    init     := Init;
    eval     := Eval;
    evalHint := EvalHint;
  END;

PROCEDURE Init(t : T; from : LRScalarField.T) : T =
  BEGIN
    t.base := from;
    t.tbl  := NEW(LRVectorLRTbl.Default).init();
    t.open := NEW(LRVectorRefTbl.Default).init();
    t.mu   := NEW(MUTEX);
    RETURN t
  END Init;

PROCEDURE EvalHint(t : T; pp : LRVector.T) =
  VAR
    dummyref : REFANY;
    dummylr  : LONGREAL;
    cl       : Closure;
    p        : LRVector.T;
  BEGIN
    LOCK t.mu DO
      p := LRVector.Copy(pp);
      IF NOT (t.open.get(p, dummyref) OR t.tbl.get(p, dummylr)) THEN
        cl := NEW(Closure, t := t, p := p, c := NEW(Thread.Condition));
        EVAL Thread.Fork(cl);
        EVAL t.open.put(p, cl)
      END
    END
  END EvalHint;

PROCEDURE Eval(t : T; pp : LRVector.T) : LONGREAL =
  VAR
    ref : REFANY;
    res : LONGREAL;
    p   : LRVector.T;
  BEGIN
    LOCK t.mu DO
      p        := LRVector.Copy(pp);
    END;
    t.evalHint(p);
    LOCK t.mu DO
      IF NOT t.tbl.get(p, res) THEN
        WITH hadIt = t.open.get(p, ref),
             cl    = NARROW(ref, Closure) DO
          <*ASSERT hadIt*>
          WHILE NOT t.tbl.get(p, res) DO
            Thread.Wait(t.mu, cl.c)
          END
        END
      END
    END;
    RETURN res
  END Eval;

TYPE
  Closure = Thread.Closure OBJECT
    t : T;
    p : LRVector.T;
    c : Thread.Condition;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Apply(cl : Closure) : REFANY =
  VAR
    ref : REFANY;
  BEGIN
    WITH res = cl.t.base.eval(cl.p) DO
      LOCK cl.t.mu DO
        WITH hadIt  = cl.t.open.delete(cl.p, ref),
             hadIt2 = cl.t.tbl.put(cl.p, res) DO
          <*ASSERT hadIt*>
          <*ASSERT ref = cl*>
          <*ASSERT NOT hadIt2*>
        END
      END;
      Thread.Broadcast(cl.c)
    END;
    RETURN NIL
  END Apply;
  
BEGIN END LRScalarFieldPll.
