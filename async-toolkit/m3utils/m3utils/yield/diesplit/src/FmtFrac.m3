(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE FmtFrac;
IMPORT Fmt;
IMPORT Debug;

CONST DoDebug = FALSE;
      
PROCEDURE Frac(x : LONGREAL; base : Fmt.Base) : TEXT =
  VAR
    eps := 1.0d-9;
  BEGIN
    FOR i := 1 TO 2048 DO
      WITH mul = x * FLOAT(i, LONGREAL),
           rnd = ROUND(mul),
           absdiff = ABS(mul - FLOAT(rnd, LONGREAL) ) DO
        IF DoDebug THEN
          Debug.Out(Fmt.F("i %s mul %s rnd %s absdiff %s",
                          Fmt.Int(i),
                          Fmt.LongReal(mul),
                          Fmt.Int(rnd),
                          Fmt.LongReal(absdiff)))
        END;

        IF absdiff <= eps THEN
          IF i = 1 THEN
            RETURN Fmt.Int(rnd, base := base)
          ELSE
            RETURN Fmt.F("%s/%s",
                         Fmt.Int(rnd, base := base),
                         Fmt.Int(i, base := base))
          END
        END
      END
    END;
    RETURN Fmt.LongReal(x) (* base 10, can't be helped? *)
  END Frac;

BEGIN END FmtFrac.
