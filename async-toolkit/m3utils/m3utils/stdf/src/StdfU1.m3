MODULE StdfU1;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Int;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure } =
  BEGIN
    IF len < 1 THEN
      RAISE StdfE.E("short read")
    ELSE
      TRY
        t := ORD(Rd.GetChar(rd));
        DEC(len,Bytes);
      EXCEPT
        Rd.EndOfFile => RAISE StdfE.E("EOF")
      END
    END
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Int(t)
  END Format;

BEGIN END StdfU1.
