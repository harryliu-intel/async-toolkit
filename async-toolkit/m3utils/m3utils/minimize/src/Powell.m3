(* $Id$ *)

MODULE Powell;
IMPORT Matrix, Compress;
IMPORT Debug;

CONST ItMax = 200;

PROCEDURE Minimize(VAR p : Matrix.Vector;
                   VAR xi : Matrix.T;
                   ftol : LONGREAL;
                   func : Compress.MultiFunc) : LONGREAL =
  VAR 
    pt, ptt, xit := NEW(Matrix.Vector, NUMBER(p^));
    t, fptt, fp, del : LONGREAL;
    ibig : INTEGER;
    fret := func(p);
    iter : INTEGER;
  BEGIN
    <* ASSERT NUMBER(xi^)  = NUMBER(p^) AND NUMBER(xi[0]) = NUMBER(p^) *>
    LOOP
      fp := fret;
      ibig := 0;
      del := 0.0d0;
      FOR i := FIRST(p^) TO LAST(p^) DO
        FOR j := FIRST(p^) TO LAST(p^) DO
          fptt := fret;
          fret := Compress.LinMin(p,xit,func);
          IF ABS(fptt - fret) > del THEN
            del := ABS(fptt - fret);
            ibig := i
          END
        END
      END;

      IF 2.0d0 * ABS(fp-fret) <= ftol*(ABS(fp)+ABS(fret)) THEN RETURN fret END;

      IF iter = ItMax THEN 
        Debug.Error("Too many iterations in Powell.Minimize.") 
      END;

      FOR j := FIRST(p^) TO LAST(p^) DO
        ptt[j] := 3.0d0 * p[j] - pt[j];
        xit[j] := p[j] - pt[j];
        pt[j] := p[j]
      END;
      
      fptt := func(ptt);
      IF fptt < fp THEN
        t := 2.0d0*(fp - 2.0d0*fret + fptt)*(fp-fret-del)*(fp-fret-del) - 
             del*(fp-fptt)*(fp-fptt)
      END;

      IF t < 0.0d0 THEN
        fret := Compress.LinMin(p,xit,func);
        FOR j := FIRST(p^) TO LAST(p^) DO xi[j,ibig] := xit[j] END
      END;

      INC(iter);
    END (* LOOP *)


  END Minimize;
                  
BEGIN END Powell.
