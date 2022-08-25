MODULE TriConfig;
IMPORT P3;
IMPORT Fmt; FROM Fmt IMPORT F;

CONST LR = Fmt.LongReal;

PROCEDURE Minus(READONLY a, b : T) : T =
  BEGIN
    RETURN T { "xxxx", a.temp - b.temp, P3.Minus(a.V, b.V), P3.Minus(a.f, b.f) }
  END Minus;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN F("%s temp %s V %s f %s",
             t.corner, LR(t.temp), P3.Format(t.V), P3.Format(t.f))
  END Format;

BEGIN END TriConfig.
