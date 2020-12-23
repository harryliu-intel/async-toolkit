MODULE Interpolate;
IMPORT Math;

(* X and Y are swapped from what might be expected in this file *)

PROCEDURE OverCard(READONLY spec  : ARRAY OF CARDINAL;
                   READONLY sigma : ARRAY OF LONGREAL;
                   READONLY meas  : CARDINAL) : LONGREAL =

  PROCEDURE Do(i0, i1 : CARDINAL) : LONGREAL =
    BEGIN
      WITH x0 = sigma[i0],
           x1 = sigma[i1],
           y0 = FLOAT(spec [i0], LONGREAL),
           y1 = FLOAT(spec [i1], LONGREAL),
           m  = FLOAT(meas, LONGREAL) DO
        RETURN x0 + (x1 - x0) * (m - y0) / (y1 - y0)
      END
    END Do;
    
  BEGIN
    FOR j := 1 TO LAST(spec) DO
      IF meas <= spec[j] THEN
        RETURN Do(j - 1, j)
      END
    END;
    RETURN Do(LAST(spec) - 1, LAST(spec))
  END OverCard;
    
PROCEDURE OverLR(READONLY spec  : ARRAY OF LONGREAL;
                 READONLY sigma : ARRAY OF LONGREAL;
                 READONLY meas  : LONGREAL) : LONGREAL =

  PROCEDURE Do(i0, i1 : CARDINAL) : LONGREAL =
    BEGIN
      WITH x0 = sigma[i0],
           x1 = sigma[i1],
           y0 = FLOAT(spec [i0], LONGREAL),
           y1 = FLOAT(spec [i1], LONGREAL),
           m  = FLOAT(meas, LONGREAL) DO
        RETURN x0 + (x1 - x0) * (m - y0) / (y1 - y0)
      END
    END Do;
    
  BEGIN
    FOR j := 1 TO LAST(spec) DO
      IF meas <= spec[j] THEN
        RETURN Do(j - 1, j)
      END
    END;
    RETURN Do(LAST(spec) - 1, LAST(spec))
  END OverLR;

PROCEDURE Exp(READONLY spec  : ARRAY OF LONGREAL;
              READONLY sigma : ARRAY OF LONGREAL;
              READONLY meas  : LONGREAL) : LONGREAL =

  PROCEDURE Do(i0, i1 : CARDINAL) : LONGREAL =
    BEGIN
      WITH x0 = Math.log(sigma[i0]),
           x1 = Math.log(sigma[i1]),
           y0 = (spec [i0]),
           y1 = (spec [i1]),
           m  = FLOAT(meas, LONGREAL) DO
        RETURN Math.exp(x0 + (x1 - x0) * (m - y0) / (y1 - y0))
      END
    END Do;
    
  BEGIN
    FOR j := 1 TO LAST(spec) DO
      IF meas <= spec[j] THEN
        RETURN Do(j - 1, j)
      END
    END;
    RETURN Do(LAST(spec) - 1, LAST(spec))
  END Exp;

BEGIN END Interpolate.
