(* $Id$ *)

GENERIC MODULE SXFuncOps(Arg, Result);
IMPORT SXClass;
IMPORT XTime AS Time;
FROM SX IMPORT Uninitialized;
IMPORT SX;
IMPORT SXInt;

TYPE 
  Unary = OpResult OBJECT
    f : F1;
    a : Arg.T;
  OVERRIDES
    recalc := UnaryRecalc;
  END;

  Binary = OpResult OBJECT
    f : F2;
    a, b : Arg.T;
  OVERRIDES
    recalc := BinaryRecalc;
  END;

  BinaryShortCircuit = OpResult OBJECT
    f : F2;
    a, b : Arg.T;
    ssOp : Arg.Base;
    ssRes : Result.Base;
  OVERRIDES
    recalc := BinarySSRecalc;
  END;

  NAry = OpResult OBJECT
    mu : MUTEX;
    f : FN;
    a : REF ARRAY OF Arg.T;
    av : REF ARRAY OF Arg.Base; (* temporary storage, allocated once only *)
  OVERRIDES
    recalc := NAryRecalc;
  END;

  IAry = OpResult OBJECT
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

PROCEDURE BinarySSRecalc(b : BinaryShortCircuit; when : Time.T) : BOOLEAN = 
  BEGIN
    TRY
      IF b.a.value() = b.ssOp THEN RETURN b.update(b.ssRes, when) END
    EXCEPT
      Uninitialized => (* skip *)
    END;

    TRY
      IF b.b.value() = b.ssOp THEN RETURN b.update(b.ssRes, when) END
    EXCEPT
      Uninitialized => (* skip *)
    END;

    TRY
      RETURN b.update(b.f(b.a.value(),b.b.value()),when)
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END BinarySSRecalc;

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

PROCEDURE UnaryFunc(a : Arg.T; f : F1; opName : TEXT := NIL) : Result.T =
  BEGIN 
    WITH res = NEW(Unary, opName := opName, f := f, a := a).init() DO
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

PROCEDURE BinaryFunc(a, b : Arg.T; f : F2; opName : TEXT := NIL) : Result.T =
  BEGIN 
    WITH res = NEW(Binary, opName := opName, f := f, a := a, b := b).init() DO
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

PROCEDURE BinarySymmetricShortCircuitFunc(a, b : Arg.T; f : F2; 
                                          ssOp : Arg.Base; ssRes : Result.Base;
                                          opName : TEXT := NIL) : Result.T =
  BEGIN 
    WITH res = NEW(BinaryShortCircuit, 
                   opName := opName, 
                   ssOp := ssOp, ssRes := ssRes,
                   f := f, a := a, b := b).init() DO
      SX.Lock( SX.Array { a, b }) ;
      TRY
        EVAL BinarySSRecalc(res, MAX(a.updated,b.updated));
        a.depends(res); b.depends(res)
      FINALLY
        SX.Unlock(SX.Array { a,b })
      END;
      RETURN res
    END
  END BinarySymmetricShortCircuitFunc;

PROCEDURE NAryFunc(READONLY a : ARRAY OF Arg.T; f : FN; opName : TEXT := NIL) : Result.T =
  VAR 
    max := FIRST(Time.T);
  BEGIN 
    WITH res = NARROW(NEW(NAry, 
                          opName := opName,
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
                   READONLY a : ARRAY OF Arg.T; f : FI;
                   opName : TEXT) : Result.T =
  VAR 
    max := FIRST(Time.T);
  BEGIN 
    WITH res = NARROW(NEW(IAry, 
                          opName := opName,
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
