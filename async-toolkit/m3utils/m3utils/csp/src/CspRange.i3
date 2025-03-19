INTERFACE CspRange;
IMPORT CspExpression;
IMPORT SchemePair;

TYPE
  T = RECORD
    min, max : CspExpression.T; (* may not be NIL *)
  END;

CONST Brand = "CspRange";

PROCEDURE Lisp(READONLY t : T) : SchemePair.T;
  
END CspRange.
