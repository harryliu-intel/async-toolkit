MODULE StdfVn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Text;
IMPORT StdfC1;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    IF len < 1 THEN
      RAISE StdfE.E("short read")
    ELSE
      VAR
        rlen := ORD(Rd.GetChar(rd));
      BEGIN
        DEC(len);
        t := NEW(T, rlen);
        FOR i := FIRST(t^) TO LAST(t^) DO
          StdfC1.Parse(rd, len, SUBARRAY(t^,i,1))
        END
      END
    END
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Text.FromChars(t^)
  END Format;

BEGIN END StdfVn.
