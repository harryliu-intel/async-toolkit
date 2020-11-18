MODULE StdfU1;
IMPORT Rd, StdfRd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Int;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    IF len = 0 THEN RETURN END;
    t := StdfRd.U1(rd, len)
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Int(t)
  END Format;

BEGIN END StdfU1.
