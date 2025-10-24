MODULE Tcs;

PROCEDURE Interpolate(lambda : LONGREAL) : T =
  VAR
    nm := lambda * 1.0d9;
    lo := FIRST(Data);
    hi := LAST(Data) + 1; (* limit of range (out of bounds) *)
  BEGIN
    IF nm < Data[lo].lambdaNm OR
       nm > Data[hi - 1].lambdaNm THEN
      RETURN T { nm, RData { 0.0d0, .. } }
    ELSE
      WHILE lo < hi DO
        WITH i  = lo + (hi - lo) DIV 2,
             ai = Data[i].lambdaNm DO
          IF nm = ai THEN
            lo := i + 1;
            hi := lo;
            EXIT
          ELSIF nm > ai THEN
            lo := i + 1
          ELSE
            hi := i
          END
        END
      END;

      <*ASSERT lo = hi*>

      DEC(lo);
      
      <*ASSERT nm >= Data[lo].lambdaNm*>
      <*ASSERT lo = LAST(Data) OR nm < Data[hi].lambdaNm *>

      IF nm = Data[lo].lambdaNm THEN
        RETURN Data[lo]
      ELSE
        WITH l   = Data[lo],
             h   = Data[lo + 1],
             dx  = h.lambdaNm - l.lambdaNm,
             dx0 = nm         - l.lambdaNm,
             hr  = dx0 / dx,
             lr  = 1.0d0 - hr DO
          VAR
            res : RData;
          BEGIN
            FOR i := FIRST(RData) TO LAST(RData) DO
              res[i] := lr * l.r[i] + hr * h.r[i]
            END;
            
            RETURN T { nm, res }
          END
        END
      END
    END
  END Interpolate;

PROCEDURE R(lambda : LONGREAL) : RData =
  BEGIN
    RETURN Interpolate(lambda).r
  END R;
  
BEGIN END Tcs.
