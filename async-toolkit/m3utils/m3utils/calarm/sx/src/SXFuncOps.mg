(* $Id$ *)

GENERIC MODULE SXFuncOps(Arg, Result);
IMPORT SXClass;
IMPORT XTime AS Time;
FROM SX IMPORT Uninitialized;
IMPORT SX;
IMPORT SXInt;
IMPORT SXIterator;

TYPE 
  Unary = UnaryRoot OBJECT
    f : F1;
  OVERRIDES
    recalc := UnaryRecalc;
  END;

  UnaryRoot = OpResult OBJECT
    a : Arg.T;
  OVERRIDES
    dependsOn := UnaryDepends;
  END;

  BinaryRoot = OpResult OBJECT
    a, b : Arg.T;
  OVERRIDES
    dependsOn := BinaryDepends;
  END;

  Binary = BinaryRoot OBJECT
    f : F2;
  OVERRIDES
    recalc := BinaryRecalc;
  END;

  BinaryShortCircuit = BinaryRoot OBJECT
    f : F2;
    ssOp : Arg.Base;
    ssRes : Result.Base;
  OVERRIDES
    recalc := BinarySSRecalc;
  END;

  NAryRoot = OpResult OBJECT
    a  : REF ARRAY OF Arg.T;
  OVERRIDES
    dependsOn := NAryDepends;
  END;

  NAry = NAryRoot OBJECT
    mu : MUTEX;
    f  : FN;
    av : REF ARRAY OF Arg.Base; (* temporary storage, allocated once only *)
  OVERRIDES
    recalc := NAryRecalc;
  END;

  IAry = NAryRoot OBJECT
    mu : MUTEX;
    f  : FI;
    i  : SXInt.T;
    a  : REF ARRAY OF Arg.T;
    av : REF ARRAY OF Arg.Base; (* temporary storage, allocated once only *)
  OVERRIDES
    recalc := IAryRecalc;
  END;

PROCEDURE UnaryDepends(b : UnaryRoot) : SXIterator.T =
  BEGIN RETURN SXIterator.One(b.a) END UnaryDepends;

PROCEDURE BinaryDepends(b : BinaryRoot) : SXIterator.T =
  BEGIN RETURN SXIterator.Two(b.a, b.b) END BinaryDepends;

PROCEDURE NAryDepends(b : NAryRoot) : SXIterator.T =
  VAR
    aa := NEW(REF ARRAY OF SX.T, NUMBER(b.a^));
  BEGIN 
    FOR i := FIRST(aa^) TO LAST(aa^) DO
      aa[i] := b.a[i]
    END;
    RETURN SXIterator.Many(aa^) 
  END NAryDepends;

(**********************************************************************)

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

(**********************************************************************)

TYPE 
  UnaryO = UnaryRoot OBJECT
    o : O1;
  OVERRIDES
    recalc := UnaryORecalc;
  END;

  BinaryO = BinaryRoot OBJECT
    o : O2;
  OVERRIDES
    recalc := BinaryORecalc;
  END;

  NAryO = NAryRoot OBJECT
    mu : MUTEX;
    o : ON;
    av : REF ARRAY OF Arg.Base; (* temporary storage, allocated once only *)
  OVERRIDES
    recalc := NAryORecalc;
  END;

PROCEDURE UnaryORecalc(u : UnaryO; when : Time.T) : BOOLEAN =
  BEGIN
    TRY
      RETURN u.update(u.o.op(u.a.value()),when)
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END UnaryORecalc;

PROCEDURE BinaryORecalc(b : BinaryO; when : Time.T) : BOOLEAN = 
  BEGIN
    TRY
      RETURN b.update(b.o.op(b.a.value(),b.b.value()),when)
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END BinaryORecalc;

PROCEDURE NAryORecalc(n : NAryO; when : Time.T) : BOOLEAN =
  BEGIN
    TRY
      (* prepare av *)
      LOCK n.mu DO
        FOR i := FIRST(n.a^) TO LAST(n.a^) DO
          n.av[i] := n.a[i].value()
        END;
        RETURN n.update(n.o.op(n.av^),when)
      END
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END NAryORecalc;      

PROCEDURE UnaryOFunc(a : Arg.T; o : O1; opName : TEXT := NIL) : Result.T =
  BEGIN 
    WITH res = NEW(UnaryO, opName := opName, o := o, a := a).init() DO
      SX.Lock(SX.Array { a });
      TRY
        EVAL UnaryORecalc(res, a.updated);
        a.depends(res)
      FINALLY
        SX.Unlock(SX.Array { a })
      END;
      RETURN res
    END
  END UnaryOFunc;

PROCEDURE BinaryOFunc(a, b : Arg.T; o : O2; opName : TEXT := NIL) : Result.T =
  BEGIN 
    WITH res = NEW(BinaryO, opName := opName, o := o, a := a, b := b).init() DO
      SX.Lock( SX.Array { a, b }) ;
      TRY
        EVAL BinaryORecalc(res, MAX(a.updated,b.updated));
        a.depends(res); b.depends(res)
      FINALLY
        SX.Unlock(SX.Array { a,b })
      END;
      RETURN res
    END
  END BinaryOFunc;

PROCEDURE NAryOFunc(READONLY a : ARRAY OF Arg.T; o : ON; opName : TEXT := NIL) : Result.T =
  VAR 
    max := FIRST(Time.T);
  BEGIN 
    WITH res = NARROW(NEW(NAryO, 
                          opName := opName,
                          mu := NEW(MUTEX),
                          o := o, 
                          a  := NEW(REF ARRAY OF Arg.T,    NUMBER(a)), 
                          av := NEW(REF ARRAY OF Arg.Base, NUMBER(a))).init(),
                      NAryO),
         sa = NEW(REF SX.Array, NUMBER(a))^ DO
      
      FOR i := FIRST(a) TO LAST(a) DO
        sa[i] := a[i];
        res.a[i] := a[i]; 
        max := MAX(max, a[i].updated)
      END;

      SX.Lock(sa);
      TRY
        EVAL NAryORecalc(res, max);
        FOR i := FIRST(a) TO LAST(a) DO
          a[i].depends(res);
        END
      FINALLY
        SX.Unlock(sa)
      END;

      RETURN res
    END
  END NAryOFunc;

BEGIN END SXFuncOps.
