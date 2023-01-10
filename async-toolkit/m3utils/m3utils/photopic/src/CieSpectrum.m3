MODULE CieSpectrum;

PROCEDURE Interpolate(lambda : LONGREAL) : T =
  VAR
    nm := lambda * 1.0d9;
    lo := FIRST(Data);
    hi := LAST(Data) + 1; (* limit of range (out of bounds) *)
  BEGIN
    IF nm < Data[lo].lambdaNm OR
       nm > Data[hi - 1].lambdaNm THEN
      RETURN T { nm, 0.0d0, 0.0d0, 0.0d0, 0.0d0 }
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
             r   = dx0 / dx,
             mr  = 1.0d0 - r DO
          RETURN T { nm,
                     r * l.photopicEff + mr * h.photopicEff,
                     r * l.photopicConv + mr * h.photopicConv,
                     r * l.scotopicEff + mr * h.scotopicEff,
                     r * l.scotopicConv + mr * h.scotopicConv }
        END
      END
    END
  END Interpolate;

PROCEDURE PhotoConv(lambda : LONGREAL) : LONGREAL =
  BEGIN
    RETURN Interpolate(lambda).photopicConv
  END PhotoConv;
  
BEGIN
  EVAL Interpolate(700.0d-9)
END CieSpectrum.
    
