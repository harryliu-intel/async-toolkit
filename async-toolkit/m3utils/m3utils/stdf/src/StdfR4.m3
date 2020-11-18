UNSAFE MODULE StdfR4;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Real;
IMPORT StdfRd;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    buff : ARRAY [0..Bytes-1] OF CHAR;
  BEGIN
    IF len = 0 THEN RAISE StdfE.Missing END;
    StdfRd.Chars(rd, len, buff);
    t := LOOPHOLE(ADR(buff),REF REAL)^;
  END Parse;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Real(t)
  END Format;

BEGIN END StdfR4.
