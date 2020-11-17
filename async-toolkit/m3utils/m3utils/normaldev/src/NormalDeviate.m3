MODULE NormalDeviate;
IMPORT Random;
IMPORT Math;

PROCEDURE Get(rand : Random.T; mean, sdev : LONGREAL) : LONGREAL =
  (* this is called a Box-Muller transformation.
     Num. Recip. in Fortran 77, sec. 7-2 *)
  VAR
    v1, v2, rsq : LONGREAL;
  BEGIN
    REPEAT
      v1 := 2.0d0 * rand.longreal(0.0d0,1.0d0)-1.0d0;
      v2 := 2.0d0 * rand.longreal(0.0d0,1.0d0)-1.0d0;
      rsq := v1*v1 + v2*v2
    UNTIL rsq > 0.0d0 AND rsq < 1.0d0;
    WITH fac = Math.sqrt(-2.0d0*Math.log(rsq)/rsq) DO
      RETURN mean + sdev*v2*fac
    END
  END Get;

BEGIN END NormalDeviate.
