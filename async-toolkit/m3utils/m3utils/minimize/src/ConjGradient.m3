(* $Id$ *)

MODULE ConjGradient;
IMPORT Matrix, Compress, Debug;
IMPORT Fmt;

CONST ItMax = 5000;
CONST Eps = 1.0d-10;

PROCEDURE Minimize(VAR p : Matrix.Vector;
                   ftol : LONGREAL;
                   func : Compress.MultiFunc;
                   dfunc : Compress.GradMultiFunc) : LONGREAL =
  VAR 
    fret : LONGREAL;
    g, h, xi := NEW(Matrix.Vector, NUMBER(p^));
    fp, dgg, gg, gam : LONGREAL;
    its := 0;
  BEGIN
    fp := func(p);
    xi := NEW(Matrix.Vector, NUMBER(p^));
    dfunc(p,xi);
    FOR j := FIRST(xi^) TO LAST(xi^) DO
      g[j] := -xi[j];
      h[j] := g[j];
      xi[j] := h[j]
    END;
    LOOP
      fret := Compress.LinMin(p,xi,func);

      (* quit if we are close enough... *)
      IF 2.0d0 * ABS(fret - fp) <= ftol * (ABS(fret) + ABS(fp) + Eps) THEN
        Debug.Out("Conjgradient.Minimize: " & Fmt.Int(its) & " iterations.");
        RETURN fret
      END;
      
      fp := func(p);
      dfunc(p,xi);
      Debug.Out("Conjgradient.Minimize: " & Fmt.LongReal(fp),100);

      gg := 0.0d0; dgg := 0.0d0;
      FOR j := FIRST(g^) TO LAST(g^) DO
        gg := gg + g[j] * g[j];
        dgg := dgg + (xi[j]+g[j])*xi[j] (* Polak-Ribiere *)
      END;
      IF gg = 0.0d0 THEN 
        Debug.Out("Conjgradient.Minimize: " & Fmt.Int(its) & " iterations.");
        RETURN fret 
      END; (* hmm.. *)

      gam := dgg/gg;
      
      FOR j := FIRST(g^) TO LAST(g^) DO
        g[j] := -xi[j];
        h[j] := g[j] + gam*h[j];
        xi[j] := h[j]
      END;
      INC(its);

      (* how quickly is it supposed to converge? *)
      IF its >= ItMax + NUMBER(p^) DIV 5 THEN EXIT END
    END;
    Debug.Error("Too many iterations in ConjGradient.Minimize.\nBest so far = " & Fmt.LongReal(fp));
    <* ASSERT FALSE *>
  END Minimize;

BEGIN END ConjGradient.
