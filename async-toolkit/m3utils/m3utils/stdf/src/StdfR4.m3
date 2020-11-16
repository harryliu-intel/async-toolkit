UNSAFE MODULE StdfR4;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Real;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure } =
  BEGIN
    IF len < Bytes THEN
      RAISE StdfE.E("short read")
    ELSE
      TRY
        VAR
          buff : ARRAY [0..Bytes-1] OF CHAR;
        BEGIN
          WITH got = Rd.GetSub(rd, buff) DO
            IF got < NUMBER(buff) THEN
              RAISE Rd.EndOfFile
            END
          END;
          DEC(len, Bytes);
          t := LOOPHOLE(ADR(buff),REF REAL)^;
        END
      EXCEPT
        Rd.EndOfFile => RAISE StdfE.E("EOF")
      END
    END
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Real(t)
  END Format;

BEGIN END StdfR4.
