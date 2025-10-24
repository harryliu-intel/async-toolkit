MODULE WLRVector;

FROM Fmt IMPORT LongReal, F;
FROM GenOptUtils IMPORT FmtP;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("{ %s y=%s w=%s }",
             FmtP(a.v), LongReal(a.y), LongReal(a.w))
  END Format;

BEGIN END WLRVector.
