MODULE Wavelet;

PROCEDURE Wavelet(VAR x, y    : ARRAY OF LONGREAL;
                  forward     : BOOLEAN) =
  VAR
    m : CARDINAL;
  BEGIN
    IF forward THEN
      m := NUMBER(x);
      REPEAT
        FOR i := 0 TO m - 1 BY 2 DO
          y[i]     := (x[i] + x[i + 1]) / 2.0d0;
          y[i + 1] := (x[i] - x[i + 1]) / 2.0d0
        END;
        m := m DIV 2;
        FOR i := 0 TO m - 1 DO
          x[i] := y [i + i];
          x[m + i] := y[i + i + 1]
        END
      UNTIL m = 1
    ELSE
      m := 1;
      REPEAT
        FOR i := 0 TO m - 1 DO
          y[i + i] := x[i];
          y[i + i + 1] := x[m + i]
        END;
        m := m + m;
        FOR i := 0 TO m - 1 BY 2 DO
          x[i]     := (y[i] + y[i + 1]);
          x[i + 1] := (y[i] - y[i + 1])
        END
      UNTIL m = NUMBER(x)
    END
  END Wavelet;

BEGIN END Wavelet.
