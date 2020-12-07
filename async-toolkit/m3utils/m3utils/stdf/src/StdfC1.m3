MODULE StdfC1;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Text;
IMPORT StdfRd;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    IF len = 0 THEN RETURN END;
    t[0] := StdfRd.Char(rd, len)
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Text.FromChar(t[0])
  END Format;

PROCEDURE Bytes(<*UNUSED*>READONLY t : T) : CARDINAL = BEGIN RETURN 1 END Bytes;

BEGIN END StdfC1.
