MODULE ArithOps;
IMPORT ArithRep;

PROCEDURE Plus(a, b : R) : R =
  BEGIN RETURN NEW(ArithRep.RPlus, a := a, b := b) END Plus;

BEGIN END ArithOps.
