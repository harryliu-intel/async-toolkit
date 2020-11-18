MODULE StdfCn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Text;
IMPORT StdfC1;
IMPORT StdfRd;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    rlen : [0..255];
  BEGIN
    IF len = 0 THEN t := Default(); RETURN END;
    rlen := StdfRd.U1(rd, len);
    t := NEW(T, rlen);
    FOR i := FIRST(t^) TO LAST(t^) DO
      StdfC1.Parse(rd, len, SUBARRAY(t^,i,1))
    END
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Text.FromChars(t^)
  END Format;

PROCEDURE Default() : T = BEGIN RETURN NEW(T, 0) END Default;

BEGIN END StdfCn.
