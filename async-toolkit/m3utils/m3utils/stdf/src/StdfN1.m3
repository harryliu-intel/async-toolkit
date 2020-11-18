MODULE StdfN1;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Word;
IMPORT StdfC1;
FROM Fmt IMPORT Int;

PROCEDURE ParseArray(rd : Rd.T; VAR len : CARDINAL; VAR t : ARRAY OF T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    c : ARRAY [0..0] OF CHAR;
  BEGIN
    FOR i := FIRST(t) TO LAST(t) DO
      IF i MOD 2 = 0 THEN
        StdfC1.Parse(rd, len, c);
        t[i] := Word.And(ORD(c[0]),15)
      ELSE
        t[i] := Word.RightShift(ORD(c[0]),4)
      END
    END
  END ParseArray;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Int(t)
  END Format;

BEGIN END StdfN1.
