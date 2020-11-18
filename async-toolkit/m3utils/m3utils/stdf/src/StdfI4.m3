MODULE StdfI4;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Int;
IMPORT StdfU4;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
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

BEGIN END StdfI4.
