MODULE FiniteInterval;
IMPORT BigInt;

PROCEDURE Construct(lo, hi : BigInt.T) : T =
  BEGIN
    RETURN T { lo, hi }
  END Construct;

BEGIN END FiniteInterval.
