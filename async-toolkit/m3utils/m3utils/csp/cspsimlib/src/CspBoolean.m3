MODULE CspBoolean;

PROCEDURE ToInteger(t : T) : INTEGER =
  BEGIN
    IF t THEN RETURN -1 ELSE RETURN 0 END
  END ToInteger;

BEGIN END CspBoolean.
