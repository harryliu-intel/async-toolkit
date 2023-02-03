MODULE TempUv;

PROCEDURE Interpolate(temp : LONGREAL) : T =
  VAR
    lo := FIRST(Data);
    hi := LAST(Data) + 1; (* limit of range (out of bounds) *)
  BEGIN
    IF temp < Data[lo].temp OR
       temp > Data[hi - 1].temp THEN
      RETURN T { temp, UV { 0.0d0, 0.0d0 } }
    ELSE
      WHILE lo < hi DO
        WITH i  = lo + (hi - lo) DIV 2,
             ai = Data[i].temp DO
          IF temp = ai THEN
            lo := i + 1;
            hi := lo;
            EXIT
          ELSIF temp > ai THEN
            lo := i + 1
          ELSE
            hi := i
          END
        END
      END;

      <*ASSERT lo = hi*>

      DEC(lo);
      
      <*ASSERT temp >= Data[lo].temp*>
      <*ASSERT lo = LAST(Data) OR temp < Data[hi].temp *>

      IF temp = Data[lo].temp THEN
        RETURN Data[lo]
      ELSE
        WITH l   = Data[lo],
             h   = Data[lo + 1],
             dx  = h.temp - l.temp,
             dx0 = temp         - l.temp,
             hr  = dx0 / dx,
             lr  = 1.0d0 - hr DO
          RETURN T { temp,
                     UV { lr * l.uv.u + hr * h.uv.u,
                          lr * l.uv.v + hr * h.uv.v }
          }
        END
      END
    END
  END Interpolate;

BEGIN END TempUv.
