(* $Id$ *)

GENERIC MODULE SXFuncOps(Arg, Result);
IMPORT SXClass;
IMPORT Time;
FROM SX IMPORT Uninitialized;
IMPORT SX;
IMPORT SXInt;

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

  IAry = Result.T OBJECT
    mu : MUTEX;
    f : FI;
    i : SXInt.T;
    a : REF ARRAY OF Arg.T;
    av : REF ARRAY OF Arg.Base; (* temporary storage, allocated once only *)
  OVERRIDES
    recalc := IAryRecalc;
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

PROCEDURE IAryRecalc(n : IAry; when : Time.T) : BOOLEAN =
  BEGIN
    TRY
      (* prepare av *)
      LOCK n.mu DO
        FOR i := FIRST(n.a^) TO LAST(n.a^) DO
          n.av[i] := n.a[i].value()
        END;
        RETURN n.update(n.f(n.i.value(),n.av^),when)
      END
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END IAryRecalc;      

(**********************************************************************)

PROCEDURE UnaryFunc(a : Arg.T; f : F1) : Result.T =
  BEGIN 
    WITH res = NEW(Unary, f := f, a := a).init() DO
      SX.Lock(SX.Array { a });
      TRY
        EVAL UnaryRecalc(res, a.updated);
        a.depends(res)
      FINALLY
        SX.Unlock(SX.Array { a })
      END;
      RETURN res
    END
  END UnaryFunc;

PROCEDURE BinaryFunc(a, b : Arg.T; f : F2) : Result.T =
  BEGIN 
    WITH res = NEW(Binary, f := f, a := a, b := b).init() DO
      SX.Lock( SX.Array { a, b }) ;
      TRY
        EVAL BinaryRecalc(res, MAX(a.updated,b.updated));
        a.depends(res); b.depends(res)
      FINALLY
        SX.Unlock(SX.Array { a,b })
      END;
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
                      NAry),
         sa = NEW(REF SX.Array, NUMBER(a))^ DO
      
      FOR i := FIRST(a) TO LAST(a) DO
        sa[i] := a[i];
        res.a[i] := a[i]; 
        max := MAX(max, a[i].updated)
      END;

      SX.Lock(sa);
      TRY
        EVAL NAryRecalc(res, max);
        FOR i := FIRST(a) TO LAST(a) DO
          a[i].depends(res);
        END
      FINALLY
        SX.Unlock(sa)
      END;

      RETURN res
    END
  END NAryFunc;

PROCEDURE IAryFunc(int : SXInt.T;
                   READONLY a : ARRAY OF Arg.T; f : FI) : Result.T =
  VAR 
    max := FIRST(Time.T);
  BEGIN 
    WITH res = NARROW(NEW(IAry, 
                          mu := NEW(MUTEX),
                          i := int,
                          f := f, 
                          a  := NEW(REF ARRAY OF Arg.T,    NUMBER(a)), 
                          av := NEW(REF ARRAY OF Arg.Base, NUMBER(a))).init(),
                      IAry),
         sa = NEW(REF SX.Array, NUMBER(a)+1)^ DO
      
      FOR i := FIRST(a) TO LAST(a) DO
        sa[i] := a[i];
        res.a[i] := a[i]; 
        max := MAX(max, a[i].updated)
      END;
      sa[LAST(sa)] := int;

      SX.Lock(sa);
      TRY
        EVAL IAryRecalc(res, max);
        FOR i := FIRST(a) TO LAST(a) DO
          a[i].depends(res);
        END
      FINALLY
        SX.Unlock(sa)
      END;

      RETURN res
    END
  END IAryFunc;

BEGIN END SXFuncOps.
