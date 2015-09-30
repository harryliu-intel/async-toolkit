INTERFACE Assertion;

TYPE
  T = RECORD
    nm         : TEXT;
    tm         : LONGREAL;
    minV, maxV : LONGREAL;
  END;

CONST Brand = "Assertion";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END Assertion.
