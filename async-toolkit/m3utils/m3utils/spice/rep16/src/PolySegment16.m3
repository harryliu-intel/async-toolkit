MODULE PolySegment16;
FROM Fmt IMPORT F, Int;
IMPORT Rep16;

PROCEDURE Format(READONLY t : T; full : BOOLEAN) : TEXT =
  BEGIN
    RETURN F("{ r=%s lo=%s n=%s }", Rep16.Format(t.r, full), Int(t.lo), Int(t.n))
  END Format;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN Rep16.Equal(a.r, b.r) AND a.lo = b.lo AND a.n = b.n
  END Equal;

BEGIN END PolySegment16.
