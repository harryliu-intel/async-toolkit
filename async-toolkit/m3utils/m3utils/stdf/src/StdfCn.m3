MODULE StdfCn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Text;
IMPORT StdfC1;
IMPORT StdfRd;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    rlen := StdfRd.U1(rd, len);
  BEGIN
    DEC(len);
    t := NEW(T, rlen);
    FOR i := FIRST(t^) TO LAST(t^) DO
      StdfC1.Parse(rd, len, SUBARRAY(t^,i,1))
    END
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Text.FromChars(t^)
  END Format;

BEGIN END StdfCn.
