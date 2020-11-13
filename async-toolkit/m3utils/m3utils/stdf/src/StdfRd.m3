MODULE StdfRd;
IMPORT Rd;

PROCEDURE GetChars(rd : Rd.T; VAR len : CARDINAL; VAR x : ARRAY OF CHAR)
  RAISES { ShortRead } =
  BEGIN
    WITH attempt = MIN(len, NUMBER(x)),
         got     = Rd.GetSub(rd, SUBARRAY(x, 0, attempt)) DO
      DEC(len, got);
      IF got < NUMBER(x) THEN RAISE ShortRead END
    END
  END GetChars;

BEGIN END StdfRd.

