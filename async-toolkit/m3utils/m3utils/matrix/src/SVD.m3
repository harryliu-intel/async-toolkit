(* $Id$ *)

MODULE SVD;
IMPORT Matrix;
FROM Math IMPORT sqrt;

PROCEDURE Sign(READONLY a, b : LONGREAL) : LONGREAL =
  BEGIN
    IF b > 0.0d0 THEN RETURN ABS(a) ELSE RETURN -ABS(a) END
  END Sign;

PROCEDURE pythag(a, b : LONGREAL) : LONGREAL =
  VAR
    absa := ABS(a);
    absb := ABS(b);
  BEGIN
    IF absa > absb THEN
      RETURN absa * sqrt(1.0d0 + (absb/absa)*(absb/absa))
    ELSE
      IF absb = 0.0d0 THEN
        RETURN 0.0d0
      ELSE
        RETURN absb * sqrt(1.0d0 + (absa/absb)*(absa/absb))
      END
    END
  END pythag;

PROCEDURE Decompose((* INOUT *) a : Matrix.T;
                    (* OUT *) w : Matrix.Vector;
                    (* OUT *) v : Matrix.T) RAISES { NoConvergence } =
  CONST
    Iters = 30;
  VAR
    m := NUMBER(a^);
    n := NUMBER(a[0]);
    g, scale, anorm := 0.0d0;
    c, f, h, s, x, y, z : LONGREAL;
    rv1 := NEW(Matrix.Vector, n);
    l, nm : CARDINAL;
    case : [1..2];
  BEGIN
    FOR i := 0 TO n-1 DO
      l := i+1;
      rv1[i] := scale * g;
      g := 0.0d0; s := 0.0d0; scale := 0.0d0;
      IF i <= m THEN
        FOR k := i TO m - 1 DO
          scale := scale + ABS(a[k,i])
        END;

        IF scale # 0.0d0 THEN
          FOR k := i TO m - 1 DO
            a[k,i] := a[k,i]/scale;
            s := s + a[k,i]*a[k,i]
          END;
          f := a[i,i];
          g := -Sign(sqrt(s),f);
          h := f*g-s;
          a[i,i] := f-g;
          FOR j := l TO n-1 DO
            s := 0.0d0;
            FOR k := i TO m - 1 DO
              s := s + a[k,i]*a[k,j]
            END;
            f := s/h;
            FOR k := i TO m - 1 DO
              a[k,j] := a[k,j]+ f*a[k,i]
            END (* 14 *) 

            (* end of p. 59 *)

          END;
          FOR k := i TO m - 1 DO
            a[k,i] := scale * a[k,i]
          END
        END
      END;
      w[i] := scale*g;
      g := 0.0d0; s := 0.0d0; scale := 0.0d0;
      IF i <= m AND i # n THEN
        FOR k := l TO n - 1 DO
          scale := scale + ABS(a[i,k])
        END;
        IF scale # 0.0d0 THEN
          FOR k := l TO n - 1 DO
            a[i,k] := a[i,k]/scale;
            s := s + a[i,k]*a[i,k]
          END;
          f := a[i,l];
          g := -Sign(sqrt(s),f);
          h := f*g-s;
          a[i,l] := f-g;
          FOR k := l TO n - 1 DO
            rv1[k] := a[i,k]/h
          END;
          FOR j := l TO m - 1 DO
            s := 0.0d0;
            FOR k := l TO n - 1 DO
              s := s+a[j,k] + s*rv1[k]
            END
          END;
          FOR k := l TO n-1 DO
            a[i,k] := scale * a[i,k]
          END
        END
      END;
      anorm := MAX(anorm,ABS(w[i])+ABS(rv1[i]))
    END;
    FOR i := n TO 0 BY -1 DO
      IF i < n THEN
        IF g # 0.0d0 THEN
          FOR j := l TO n - 1 DO
            v[j,i] := (a[i,j]/a[i,l])/g
          END;
          FOR j := l TO n - 1 DO
            s := 0.0d0;
            FOR k := l TO n - 1 DO
              s := s + a[j,k]*v[k,j]
            END;
            FOR k := l TO n - 1 DO
              v[k,j] := v[k,j]+ s*v[k,i]
            END
          END
        END;
        FOR j := l TO n - 1 DO
          v[i,j] := 0.0d0; v[j,i] := 0.0d0
        END
      END;
      v[i,i] := 1.0d0; (* end of p. 60 *)
      g := rv1[i];
      l := i
    END; (* 32 *)
    FOR i := MIN(m,n) TO 0 BY -1 DO
      l := i+1;
      g := w[i];
      FOR j := l TO n-1 DO
        a[i,j] := 0.0d0
      END;
      IF g # 0.0d0 THEN
        g := 1.0d0/g;
        FOR j := l TO n - 1 DO
          s := 0.0d0;
          FOR k := l TO m - 1 DO
            s := s + a[k,i]*a[k,j]
          END;
          f := (s/a[i,i])*g;
          FOR k := i TO m - 1 DO
            a[k,j] := a[k,j]+f*a[k,i]
          END
        END;
        FOR j := i TO m - 1 DO
          a[j,i] := a[j,i]*g
        END (* 37 *)
      ELSE
        FOR j := i TO m - 1 DO
          a [j,i] := 0.0d0
        END
      END;
      a[i,i] := a[i,i]+1.0d0
    END;
    FOR k := n TO 0 BY -1 DO
      FOR its := 1 TO Iters DO
        FOR l := k TO 0 BY -1 DO
          nm := l-1;
          IF    ABS(rv1[l])+anorm = anorm THEN 
            case := 2; EXIT
          ELSIF ABS(w[nm])+anorm = anorm THEN
            case := 1; EXIT
          END
        END;
(* 1 *) IF case = 1 THEN
          c := 0.0d0; s := 1.0d0;
          FOR i := l TO k-1 DO
            f := s*rv1[i];
            rv1[i] := c * rv1[i];
            IF ABS(f) + anorm = anorm THEN EXIT END;
            g := w[i];
            h := pythag(f,g);
            w[i] := h;
            h := 1.0d0/h;
            c := g*h;
            s := -(f*h);
            FOR j := i TO m - 1 DO
              y := a[j,nm];
              z := a[j,i];
              a[j,nm] := (y*c)+(z*s);
              a[j,i] := -(y*s)+(z*c)
            END
          END
        END;
(* 2 *) z := w[k];
        IF l = k THEN
          IF z < 0.0d0 THEN
            w[k] := -z;
            FOR j := 0 TO n - 1 DO
              v[j,k] := - v[j,k]
            END (* end of p. 61 *)
          END;
          EXIT (* goto 3 *)
        END;
        IF its = Iters THEN RAISE NoConvergence END;
        x := w[l]; nm := k-1; y := w[nm]; g := rv1[nm]; h := rv1[k];
        f := ((y-z)*(y+z)+(g-h)*(g+h))/(2.0d0*h*y);
        g := pythag(f,1.0d0);
        f := ((x-z)*(x+z)+h*((y/(f+Sign(g,f)))-h))/x;
        c := 1.0d0; s := 1.0d0;
        FOR j := l TO nm DO
          VAR
            i := j+1;
          BEGIN
            g := rv1[i]; y := w[i]; h := s*g; g := c*g; z := pythag(f,h);
            rv1[j] := z; c := f/z; s := h/z;
            f :=  (x*c)+(g*s);
            g := -(x*s)+(g*c);
            h := y*s;
            y := y*c;
            FOR jj := 0 TO n - 1 DO
              x := v[jj,j];
              z := v[jj,i];
              v[jj,j] :=  (x*c)+(z*s);
              v[jj,i] := -(x*s)+(z*c)
            END;
            z := pythag(f,h);
            w[j] := z;
            IF z # 0.0d0 THEN
              z := 1.0d0/z;
              c := f*z;
              s := h*z;
            END;
            f :=  (c*g)+(s*y);
            x := -(s*g)+(c*y);
            FOR jj := 0 TO m - 1 DO
              y := a[jj,j];
              z := a[jj,i];
              a[jj,j] :=  (y*c)+(z*s);
              a[jj,i] := -(y*s)+(z*c)
            END
          END; (* 47 *)
          rv1[l] := 0.0d0;
          rv1[k] := f;
          w[k] := x
        END (* VAR *)
      END (* 48 *)
(* 3 *)
    END (* 49 *)
  END Decompose;

PROCEDURE BackSubstitute(u : Matrix.T;
                         w : Matrix.Vector;
                         v : Matrix.T;

                         b : Matrix.Vector;
                         (* OUT *) x : Matrix.Vector) =
  VAR
    m := NUMBER(u^);
    n := NUMBER(u[0]);
    s : LONGREAL;
    tmp := NEW(REF ARRAY OF LONGREAL, n);
  BEGIN
    FOR j := 0 TO n - 1 DO
      s := 0.0d0;
      IF w[j] # 0.0d0 THEN
        FOR i := 0 TO m-1 DO
          s := s + u[i,j]*b[i]
        END;
        s := s/w[j]
      END;
      tmp[j] := s
    END; (* 12 *)
    FOR j := 0 TO n - 1 DO
      s := 0.0d0;
      FOR jj := 0 TO n - 1 DO
        s := s + v[j,jj]*tmp[jj]
      END;
      x[j] := s
    END
  END BackSubstitute;

BEGIN END SVD.
