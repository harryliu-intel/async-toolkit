(* $Id$ *)

GENERIC MODULE SXFuncOps(Arg, Result);
IMPORT SXClass;
IMPORT Time, Thread;
FROM SX IMPORT Uninitialized;

TYPE 
  Unary = Result.T OBJECT
    f : F1;
    a : Arg.T;
  OVERRIDES
    recalc := UnaryRecalc;
  END;

  Binary = Result.T OBJECT
    f : F2;
    a, b : Arg.T;
  OVERRIDES
    recalc := BinaryRecalc;
  END;

  NAry = Result.T OBJECT
    mu : MUTEX;
    f : FN;
    a : REF ARRAY OF Arg.T;
    av : REF ARRAY OF Arg.Base; (* temporary storage, allocated once only *)
  OVERRIDES
    recalc := NAryRecalc;
  END;

PROCEDURE UnaryRecalc(u : Unary; when : Time.T) : BOOLEAN =
  BEGIN
    TRY
      RETURN u.update(u.f(u.a.value()),when)
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END UnaryRecalc;

PROCEDURE BinaryRecalc(b : Binary; when : Time.T) : BOOLEAN = 
  BEGIN
    TRY
      RETURN b.update(b.f(b.a.value(),b.b.value()),when)
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END BinaryRecalc;

PROCEDURE NAryRecalc(n : NAry; when : Time.T) : BOOLEAN =
  BEGIN
    TRY
      (* prepare av *)
      LOCK n.mu DO
        FOR i := FIRST(n.a^) TO LAST(n.a^) DO
          n.av[i] := n.a[i].value()
        END;
        RETURN n.update(n.f(n.av^),when)
      END
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END NAryRecalc;      

(**********************************************************************)

PROCEDURE UnaryFunc(a : Arg.T; f : F1) : Result.T =
  BEGIN 
    WITH res = NEW(Unary, f := f, a := a).init() DO
      LOCK a.mu DO
        EVAL UnaryRecalc(res, a.updated);
        a.depends(res)
      END;
      RETURN res
    END
  END UnaryFunc;

PROCEDURE BinaryFunc(a, b : Arg.T; f : F2) : Result.T =
  BEGIN 
    WITH res = NEW(Binary, f := f, a := a, b := b).init() DO
      LOCK a.mu DO LOCK b.mu DO
        EVAL BinaryRecalc(res, MAX(a.updated,b.updated));
        a.depends(res); b.depends(res)
      END END;
      RETURN res
    END
  END BinaryFunc;

PROCEDURE NAryFunc(READONLY a : ARRAY OF Arg.T; f : FN) : Result.T =
  VAR 
    max := FIRST(Time.T);
  BEGIN 
    WITH res = NARROW(NEW(NAry, 
                          mu := NEW(MUTEX),
                          f := f, 
                          a  := NEW(REF ARRAY OF Arg.T,    NUMBER(a)), 
                          av := NEW(REF ARRAY OF Arg.Base, NUMBER(a))).init(),
                      NAry) DO
      
      FOR i := FIRST(a) TO LAST(a) DO
        res.a[i] := a[i]; 
        Thread.Acquire(a[i].mu);
        max := MAX(max, a[i].updated)
      END;

      EVAL NAryRecalc(res, max);
      FOR i := FIRST(a) TO LAST(a) DO
        a[i].depends(res);
        Thread.Release(a[i].mu)
      END;

      RETURN res
    END
  END NAryFunc;

BEGIN END SXFuncOps.
