(* $Id: Compress.m3,v 1.3 2001/10/10 07:39:55 mika Exp $ *)

MODULE Compress;
IMPORT Bracket;
IMPORT LRScalarField;
IMPORT LRVector;
IMPORT LRFunction;

CONST Tol = 2.0d-8;

TYPE
  Func = LRFunction.Default OBJECT
    pcom, xicom : LRVector.T;
    nrfunc : LRScalarField.T;
  OVERRIDES
    eval := EvalF1;
  END;

PROCEDURE EvalF1(f : Func; x : LONGREAL) : LONGREAL =
  VAR
    xt := NEW(LRVector.T, NUMBER(f.pcom^));
  BEGIN
    FOR j := FIRST(f.pcom^) TO LAST(f.pcom^) DO
      xt[j] := f.pcom[j] + x*f.xicom[j]
    END;
    RETURN f.nrfunc.eval(xt);
  END EvalF1;
  
PROCEDURE LinMin(VAR p : LRVector.T; (* initial and final point *)
                 VAR xi : LRVector.T; (* search direction, 
                                            replaced with change in p *)
                 func : LRScalarField.T) : LONGREAL (* returns min. value *) =

    
  VAR
    xmin : LONGREAL;
    bracket := Bracket.Trio { 0.0d0, 1.0d0, 2.0d0 };
    fret : LONGREAL;
    pcom, xicom : LRVector.T;
    f : Func;

  BEGIN
    pcom := NEW(LRVector.T, NUMBER(p^));
    xicom := NEW(LRVector.T, NUMBER(p^));

    f := NEW(Func, pcom := pcom, xicom := xicom, nrfunc := func);
    
    <* ASSERT NUMBER(p^) = NUMBER(xi^) AND FIRST(p^) = 0 AND FIRST(xi^) = 0 *>
    FOR j := FIRST(p^) TO LAST(p^) DO
      pcom[j] := p[j]; xicom[j] := xi[j]
    END;
    EVAL Bracket.Initial(bracket, f);
    fret := Bracket.Brent(bracket, f, Tol, xmin);
    FOR j := FIRST(p^) TO LAST(p^) DO
      xi[j] := xi[j] * xmin;
      p[j] := p[j] + xi[j];
    END;
    RETURN fret
  END LinMin;

BEGIN END Compress.
