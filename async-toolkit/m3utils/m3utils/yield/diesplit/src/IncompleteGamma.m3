MODULE IncompleteGamma;
IMPORT Math;

PROCEDURE Gammap(a, x : LONGREAL) : LONGREAL =
  VAR
    gln : LONGREAL;
  BEGIN
    <*ASSERT x >= 0.0d0*>
    <*ASSERT a >  0.0d0*>
    IF x < a + 1.0d0 THEN
      RETURN Gser(a, x, gln)
    ELSE
      RETURN 1.0d0 - Gcf(a, x, gln)
    END
  END Gammap;

PROCEDURE Gammaq(a, x : LONGREAL) : LONGREAL =
  VAR
    gln : LONGREAL;
  BEGIN
    <*ASSERT x >= 0.0d0*>
    <*ASSERT a >  0.0d0*>
    IF x < a + 1.0d0 THEN
      RETURN 1.0d0 - Gser(a, x, gln)
    ELSE
      RETURN Gcf(a, x, gln)
    END
  END Gammaq;

CONST Eps   = 3.0d-7;
      FpMin = 1.0d-30;

PROCEDURE Gser(a, x : LONGREAL; VAR gln : LONGREAL) : LONGREAL =
  CONST
    ItMax = 100;
  VAR
    ap, del, sum : LONGREAL;
  BEGIN
    gln := Math.gamma(a);
    <*ASSERT x >= 0.0d0*>
    IF x = 0.0d0 THEN RETURN 0.0d0 END;
    ap := a;
    sum := 1.0d0 / a;
    del := sum;
    FOR n := 1 TO ItMax DO
      ap := ap + 1.0d0;
      del := del * x / ap;
      sum := sum + del;
      IF ABS(del) < ABS(sum) * Eps THEN
        RETURN sum * Math.exp(-x + a*Math.log(x) - gln)
      END
    END;
    <*ASSERT FALSE*>
  END Gser;

PROCEDURE Gcf(a, x : LONGREAL; VAR gln : LONGREAL) : LONGREAL =
  CONST
    ItMax = 100;
  VAR
    an, b, c, d, del, h : LONGREAL;
  BEGIN
    gln := Math.gamma(a);
    b := x + 1.0d0 - a;
    c := 1.0d0 / FpMin;
    d := 1.0d0 / b;
    h := d;
    FOR i := 1 TO ItMax DO
      WITH li = FLOAT(i, LONGREAL) DO
        an := -li * (li - a);
        b := b + 2.0d0;
        d := an * d + b;
        IF ABS(d) < FpMin THEN d := FpMin END;
        c := b + an/c;
        IF ABS(c) < FpMin THEN c := FpMin END;
        d := 1.0d0 / d;
        del := d * c;
        h := h * del;
        IF ABS(del - 1.0d0) < Eps THEN
          RETURN Math.exp(-x + a * Math.log(x) - gln) * h
        END
      END
    END;
    <*ASSERT FALSE*>
  END Gcf;

BEGIN END IncompleteGamma.
