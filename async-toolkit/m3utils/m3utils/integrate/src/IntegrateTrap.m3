(* $Id$ *)

MODULE IntegrateTrap;

PROCEDURE Integrate(func   : Func; 
                     a , b : LONGREAL;
                     n     : CARDINAL;

                     (* s and it hold state; see p. 120 of num. rec./c *)
                     VAR s : LONGREAL;
                     VAR it : CARDINAL) =
  BEGIN
    IF n = 1 THEN
      it := 1;
      s := 0.5d0*(b-a)*(func(a) + func(b))
    ELSE
      VAR
        tnm := FLOAT(it,LONGREAL);
        del := (b-a)/tnm;
        x   := a + 0.5d0*del;
        sum := 0.0d0;
      BEGIN
        FOR j := 1 TO it DO
          sum := sum + func(x);
          x := x + del
        END;
        it := it*2;
        s := 0.5d0*(s+(b-a)*sum/tnm)
      END;
      RETURN
    END
  END Integrate;


PROCEDURE IntegrateN(func  : Func; 
                     a , b : LONGREAL;
                     n     : CARDINAL) : LONGREAL =
  VAR
    s : LONGREAL;
    it : CARDINAL;
  BEGIN
    FOR j := 1 TO n+1 DO
      Integrate(func,a,b,j,s,it)
    END;
    RETURN s
  END IntegrateN;

PROCEDURE IntegrateE(func  : Func; 
                     a , b : LONGREAL;
                     eps   : LONGREAL;
                     jmax  : CARDINAL) : LONGREAL RAISES { NoConvergence } =
  VAR
    s, olds : LONGREAL;
    it : CARDINAL;
  BEGIN
    FOR j := 1 TO jmax DO
      Integrate(func, a, b, j, s, it);
      IF j # 1 AND ABS(s-olds) < eps * ABS(olds) THEN RETURN s END;
      olds := s
    END;
    RAISE NoConvergence
  END IntegrateE;


PROCEDURE SimpsonE(func  : Func; 
                   a , b : LONGREAL;
                   eps   : LONGREAL;
                   jmax  : CARDINAL) : LONGREAL RAISES { NoConvergence } =
  VAR
    s, os : LONGREAL;
    st, ost := -1.0d30;
    it : CARDINAL;
  BEGIN
    FOR j := 1 TO jmax DO
      Integrate(func,a,b,j,st,it);
      s := (4.0d0 * st - ost) / 3.0d0;
      IF j # 1 AND ABS(s-os) < eps * ABS(os) THEN RETURN s END;

      os := s; 
      ost := st
    END;
    RAISE NoConvergence
  END SimpsonE;

BEGIN END IntegrateTrap.

