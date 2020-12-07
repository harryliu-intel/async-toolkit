MODULE StdfI2;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Int;
IMPORT StdfU2;
IMPORT Wr;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  TYPE
    U = StdfU2.T;
  VAR
    u : U;
  BEGIN
    StdfU2.Parse(rd, len, u);
    IF u <= LAST(T) THEN
      t := u
    ELSE
      t := u - LAST(U) - 1 
    END
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Int(t)
  END Format;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure } =
  VAR
    u : StdfU2.T;
  BEGIN
    IF t >= 0 THEN
      u := t
    ELSE
      u := t + NUMBER(T)
    END;
    StdfU2.Write(wr, u)
  END Write;

PROCEDURE Bytes(<*UNUSED*>READONLY t : T) : CARDINAL = BEGIN RETURN 2 END Bytes;

BEGIN END StdfI2.
