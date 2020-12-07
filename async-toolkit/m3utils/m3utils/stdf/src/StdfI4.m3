MODULE StdfI4;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Int;
IMPORT StdfU4;
IMPORT Wr;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  TYPE
    U = StdfU4.T;
  VAR
    u : U;
  BEGIN
    StdfU4.Parse(rd, len, u);
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
    u : StdfU4.T;
  BEGIN
    IF t >= 0 THEN
      u := t
    ELSE
      u := t + NUMBER(T)
    END;
    StdfU4.Write(wr, u)
  END Write;
  
PROCEDURE Bytes(<*UNUSED*>READONLY  t : T) : CARDINAL = BEGIN RETURN 4 END Bytes;

BEGIN END StdfI4.
