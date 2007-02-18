(* $Id$ *)

GENERIC MODULE SXFuncOps(Arg, Result);
IMPORT SXClass;
IMPORT Time;
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

BEGIN END SXFuncOps.
