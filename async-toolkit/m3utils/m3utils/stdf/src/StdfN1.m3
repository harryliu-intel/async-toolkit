MODULE StdfN1;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Word;
FROM Fmt IMPORT Int;
IMPORT StdfRd;

PROCEDURE ParseArray(rd : Rd.T; VAR len : CARDINAL; VAR t : ARRAY OF T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    u : [0..255];
  BEGIN
    IF len = 0 THEN RETURN END;
    FOR i := FIRST(t) TO LAST(t) DO
      IF i MOD 2 = 0 THEN
        u := StdfRd.U1(rd, len);
        t[i] := Word.And(u,15)
      ELSE
        t[i] := Word.RightShift(u,4)
      END
    END
  END ParseArray;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Int(t)
  END Format;

BEGIN END StdfN1.
