MODULE StdfU2;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Int;
IMPORT StdfRd;
IMPORT Wr, StdfWr;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    IF len = 0 THEN RETURN END;
    t :=           StdfRd.U1(rd, len);
    t := t + 256 * StdfRd.U1(rd, len)
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Int(t)
  END Format;

PROCEDURE Write(wr : Wr.T; READONLY u : T)
  RAISES { Thread.Alerted, Wr.Failure } =
  VAR
    t := u;
  BEGIN
    StdfWr.U1(wr, t MOD 256);
    t := t DIV 256;
    StdfWr.U1(wr, t MOD 256);
  END Write;

PROCEDURE Bytes(<*UNUSED*>READONLY t : T) : CARDINAL = BEGIN RETURN 2 END Bytes;

BEGIN END StdfU2.
