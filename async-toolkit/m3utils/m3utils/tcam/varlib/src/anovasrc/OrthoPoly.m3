(* $Id: OrthoPoly.m3,v 1.3 2005/05/03 01:27:24 mika Exp $ *)

MODULE OrthoPoly;
IMPORT Math;

(* Emerson, Biometrics, Vol.24 no. 3 (Sept. 1968), 695-701. *)

PROCEDURE Values(READONLY x, w : ARRAY OF LONGREAL;
                 VAR p : ARRAY OF ARRAY OF LONGREAL;
                 maxOrder : INTEGER) =

  PROCEDURE ComputeA() =
    VAR 
      sum := 0.0d0;
    BEGIN
      FOR i := FIRST(x) TO LAST(x) DO
        sum := sum + w[i] * q[i] * q[i]
      END;
      A := Math.sqrt(sum)
    END ComputeA;

  PROCEDURE Computep() = 
    BEGIN
      FOR i := FIRST(x) TO LAST(x) DO
        p[order,i] := q[i]/A
      END;
      INC(order)
    END Computep;

  PROCEDURE ComputeB() =
    VAR
      num := 0.0d0;
      den := 0.0d0;
    BEGIN
      FOR i := FIRST(x) TO LAST(x) DO
        num := num + x[i] * w[i] * q[i] * q[i];
        den := den + w[i] * q[i] * q[i]
      END;
      B := num/den
    END ComputeB;

  PROCEDURE Computeq() =
    BEGIN
      FOR i := FIRST(x) TO LAST(x) DO
        q[i] := (x[i] - B) * p[order-1,i];
        IF order > 1 THEN
          q[i] := q[i] - A * p[order-2,i]
        END
      END
    END Computeq;

  VAR
    q := NEW(REF ARRAY OF LONGREAL, NUMBER(x));
    A : LONGREAL;
    B : LONGREAL;
    order := 0;
  BEGIN
    IF maxOrder < 0 THEN maxOrder := LAST(p) END;

    FOR i := FIRST(q^) TO LAST(q^) DO q[i] := 1.0d0 END;

    ComputeA();
    Computep();

    WHILE order <= maxOrder DO
      ComputeB();
      Computeq();
      ComputeA();
      Computep()
    END
    
  END Values;

BEGIN END OrthoPoly.
