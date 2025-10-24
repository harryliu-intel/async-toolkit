MODULE Euclid;

PROCEDURE GCD(a, b : CARDINAL) : CARDINAL =
  BEGIN
    LOOP
      <*ASSERT a#0 OR b#0*>
      IF    a = 0 THEN
        RETURN b
      ELSIF b = 0 THEN
        RETURN a
      ELSIF a < b THEN 
        b := b MOD a
      ELSIF a > b THEN
        a := a MOD b
      ELSE
        RETURN a
      END
    END
  END GCD;

PROCEDURE Lowest(n, d : INTEGER; VAR nn, dd : INTEGER) =
  BEGIN
    WITH gcd = GCD(ABS(n),ABS(d)) DO
      nn := n DIV gcd;
      dd := d DIV gcd
    END
  END Lowest;

BEGIN END Euclid.
