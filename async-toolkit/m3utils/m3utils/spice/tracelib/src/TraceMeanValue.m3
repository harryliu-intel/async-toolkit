MODULE TraceMeanValue EXPORTS Trace;

PROCEDURE MeanValue(READONLY timea, nodea : ARRAY OF LONGREAL;
                    startTime := FIRST(LONGREAL);
                    endTime   := LAST (LONGREAL)) : LONGREAL =
  VAR
    sum := 0.0d0;
  BEGIN
    (* integrate using midpoint rule *)
    FOR i := FIRST(timea) TO LAST(timea) - 1 DO
      WITH t0 = timea[i],
           t1 = timea[i + 1],
           dt = t1 - t0,
           y0 = nodea[i],
           y1 = nodea[i + 1],
           mp = 0.5d0*(y0 + y1) DO
        IF startTime <= t1 AND endTime >= t0 THEN
          (* first, assume entire segment is included *)
          sum := sum + mp * dt ;

          (* then subtract off start or end if it should not be included *)
          IF startTime > t0 THEN
            (* integrate just the beginning with its own m.p.r. *)
            WITH ddt = startTime - t0,

                 dy  = ddt / dt * (y1 - y0),
                 yp  = y0 + dy,
                 mpp = 0.5d0*(y0 + yp) DO
              sum := sum - mpp * ddt
            END
          END;

          IF endTime < t1 THEN
            (* integrate just the beginning with its own m.p.r. *)
            WITH ddt = t1 - endTime,

                 dy  = ddt / dt * (y1 - y0),
                 yp  = y1 - dy,
                 mpp = 0.5d0*(y1 + yp) DO
              sum := sum - mpp * ddt
            END
          END
        END
      END
    END;

    (* divide by length of integrated interval *)
    VAR
      len : LONGREAL := 0.0d0;
    BEGIN
      len := len - MAX(startTime, timea[0]);
      len := len + MIN(endTime,   timea[LAST(timea)]);

      RETURN sum / len
    END
  END MeanValue;

BEGIN END TraceMeanValue.
