(* $Id$ *)

MODULE Compress;
IMPORT Matrix;
IMPORT Bracket;

CONST Tol = 2.0d-4;

(* mutex and therewith protected globals *)
VAR mu := NEW(MUTEX);
VAR pcom, xicom : Matrix.Vector; 
VAR nrfunc : MultiFunc;

PROCEDURE F1Dim(x : LONGREAL) : LONGREAL =
  VAR
    xt := NEW(Matrix.Vector, NUMBER(pcom^));
  BEGIN
    FOR j := FIRST(pcom^) TO LAST(pcom^) DO xt[j] := pcom[j] + x*xicom[j] END;
    RETURN nrfunc(xt);
  END F1Dim;

PROCEDURE LinMin(VAR p : Matrix.Vector; (* initial and final point *)
                 VAR xi : Matrix.Vector; (* search direction, 
                                            replaced with change in p *)
                 func : MultiFunc) : LONGREAL (* returns min. value *) =
  VAR
    xmin : LONGREAL;
    bracket := Bracket.Trio { 0.0d0, 1.0d0, 2.0d0 };
    fret : LONGREAL;
  BEGIN
    LOCK mu DO
      pcom := NEW(Matrix.Vector, NUMBER(p^));
      xicom := NEW(Matrix.Vector, NUMBER(p^));
      nrfunc := func;

      <* ASSERT NUMBER(p^) = NUMBER(xi^) AND FIRST(p^) = 0 AND FIRST(xi^) = 0 *>
      FOR j := FIRST(p^) TO LAST(p^) DO
        pcom[j] := p[j]; xicom[j] := xi[j]
      END;
      EVAL Bracket.Initial(bracket, F1Dim);
      fret := Bracket.Brent(bracket, F1Dim, Tol, xmin);
      FOR j := FIRST(p^) TO LAST(p^) DO
        xi[j] := xi[j] * xmin;
        p[j] := p[j] + xi[j];
      END
    END; (* LOCK mu ... *)
    RETURN fret
  END LinMin;

BEGIN END Compress.
