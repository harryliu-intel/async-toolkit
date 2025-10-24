(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE NormalDeviate;
IMPORT Random;
IMPORT Math;

PROCEDURE Trunc(rand                         : Random.T;
                mean, sdev, trunclo, trunchi : LONGREAL) : LONGREAL =
  VAR
    n1, n2 : LONGREAL;
  BEGIN
    LOOP
      Get2(rand, mean, sdev, n1, n2);
      IF trunclo <= n1 AND n1 <= trunchi THEN
        RETURN n1
      END;
      IF trunclo <= n2 AND n2 <= trunchi THEN
        RETURN n2
      END
    END
  END Trunc;

PROCEDURE Get(rand : Random.T; mean, sdev : LONGREAL) : LONGREAL =
  VAR
    dummy, n2 : LONGREAL;
  BEGIN
    Get2(rand, mean, sdev, dummy, n2);
    RETURN n2
  END Get;

PROCEDURE Get2(rand : Random.T; mean, sdev : LONGREAL; VAR d1, d2 : LONGREAL) =
  (* this is called a Box-Muller transform.
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
      d1 := mean + sdev*v1*fac;
      d2 := mean + sdev*v2*fac
    END
  END Get2;

BEGIN END NormalDeviate.
