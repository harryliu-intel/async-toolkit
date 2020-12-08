MODULE StdfN1;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Word;
FROM Fmt IMPORT Int;
IMPORT StdfRd;
IMPORT Wr, StdfWr;

PROCEDURE ParseArray(rd : Rd.T; VAR len : CARDINAL; VAR t : ARRAY OF T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
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

PROCEDURE BytesArray(READONLY t : ARRAY OF T) : CARDINAL =
  BEGIN
    RETURN (NUMBER(t) - 1) DIV 2 + 1
  END BytesArray;
  
PROCEDURE WriteArray(wr : Wr.T; READONLY t : ARRAY OF T)
  RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    b : [0..255];
  BEGIN
    FOR i := 0 TO LAST(t) BY 2 DO
      IF i + 1 <= LAST(t) THEN
        b := Word.Or(t[i], Word.LeftShift(t[i + 1], 4))
      ELSE
        b := t[i]
      END;
      StdfWr.U1(wr, b)
    END
  END WriteArray;
  
BEGIN END StdfN1.
