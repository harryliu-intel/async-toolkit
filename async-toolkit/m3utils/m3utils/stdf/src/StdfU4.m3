MODULE StdfU4;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Int;
IMPORT StdfRd;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    t :=     StdfRd.U1(rd, len);
    t := t + StdfRd.U1(rd, len) * 256;
    t := t + StdfRd.U1(rd, len) * 256 * 256;
    t := t + StdfRd.U1(rd, len) * 256 * 256 * 256;
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Int(t)
  END Format;

BEGIN END StdfU4.
