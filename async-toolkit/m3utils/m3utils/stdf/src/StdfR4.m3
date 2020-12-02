UNSAFE MODULE StdfR4;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Real;
IMPORT StdfRd;
IMPORT Wr, StdfWr;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    buff : ARRAY [ 0 .. Bytez - 1 ] OF CHAR;
  BEGIN
    IF len = 0 THEN RETURN END;
    StdfRd.Chars(rd, len, buff);
    t := LOOPHOLE(ADR(buff),REF REAL)^;
  END Parse;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure } =
  VAR
    buff : ARRAY [ 0 .. Bytez - 1 ] OF CHAR;
  BEGIN
    LOOPHOLE(ADR(buff),REF REAL)^ := t;
    StdfWr.Chars(wr, buff)
  END Write;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Real(t)
  END Format;

BEGIN END StdfR4.
