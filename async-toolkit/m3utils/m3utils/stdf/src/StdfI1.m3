MODULE StdfI1;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Int;
IMPORT StdfU1;
IMPORT Wr;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  TYPE
    U = StdfU1.T;
  VAR
    u : U;
  BEGIN
    StdfU1.Parse(rd, len, u);
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
    u : StdfU1.T;
  BEGIN
    IF t >= 0 THEN
      u := t
    ELSE
      u := t + NUMBER(T)
    END;
    StdfU1.Write(wr, u)
  END Write;

BEGIN END StdfI1.
