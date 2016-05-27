INTERFACE Assertion;

TYPE
  T = RECORD
    nm         : TEXT;
    tm         : LONGREAL;
    minV, maxV : LONGREAL;
    offset     : LONGREAL;
  END;

CONST Brand = "Assertion";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END Assertion.
