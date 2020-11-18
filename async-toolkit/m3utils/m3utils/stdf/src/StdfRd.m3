MODULE StdfRd;
IMPORT Rd;
IMPORT StdfE;
IMPORT Thread;

PROCEDURE Chars(rd : Rd.T; VAR len : CARDINAL; VAR x : ARRAY OF CHAR)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    WITH attempt = MIN(len, NUMBER(x)),
         got     = Rd.GetSub(rd, SUBARRAY(x, 0, attempt)) DO
      DEC(len, got);
      IF attempt < NUMBER(x) THEN RAISE StdfE.E("short read") END;
      IF got < attempt THEN RAISE Rd.EndOfFile END;
    END
  END Chars;

PROCEDURE Char(rd : Rd.T; VAR len : CARDINAL) : CHAR
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    IF len < 1 THEN
      RAISE StdfE.E("short read")
    END;
    
    WITH c = Rd.GetChar(rd) DO
      RETURN c
    END
  END Char;

PROCEDURE U1(rd : Rd.T; VAR len : CARDINAL) : [0..255]
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    RETURN ORD(Char(rd, len))
  END U1;

BEGIN END StdfRd.

