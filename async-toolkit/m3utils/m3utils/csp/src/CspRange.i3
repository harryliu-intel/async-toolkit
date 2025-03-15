INTERFACE CspRange;
IMPORT CspExpression;

TYPE
  T = RECORD
    min, max : CspExpression.T;
  END;

CONST Brand = "CspRange";

END CspRange.
