MODULE FreqTable;
IMPORT ArithProbability;
IMPORT Rd;
IMPORT ArithBits AS Bits;
IMPORT Word;
IMPORT Thread;
IMPORT Text;

PROCEDURE GetProbability(READONLY cum : Cum; c : CHAR) : ArithProbability.T =
  BEGIN
    RETURN ArithProbability.T { cum[ORD(c)], cum[ORD(c) + 1], cum[LAST(cum)] }
  END GetProbability;

PROCEDURE Accumulate(READONLY t : T; VAR cum : Cum) =
  BEGIN
    cum := Cum { 0, .. };

    FOR i := FIRST(cum) + 1 TO LAST(cum) DO
      cum[i] := cum[i - 1] + MAX(t[i - 1], 1);
    END
  END Accumulate;

TYPE
  CardArr = ARRAY [ FIRST(T) .. LAST(T) ] OF CARDINAL;
  
PROCEDURE FromRd(rd : Rd.T) : T RAISES { Rd.Failure, Thread.Alerted } =
  VAR
    n := 0;
    a := CardArr { 0, .. };
    res : T;
  BEGIN
    TRY
      LOOP
        WITH c = Rd.GetChar(rd) DO
          INC(a[ORD(c)]);
          INC(n)
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;
    INC(a[LAST(a)]);
    FromCards(n, a, res);
    RETURN res
  END FromRd;

PROCEDURE FromText(txt : TEXT) : T =
  VAR
    n := 0;
    a := CardArr { 0, .. };
    res : T;
  BEGIN
    FOR i := 0 TO Text.Length(txt) DO
      WITH c = Text.GetChar(txt, i) DO
        INC(a[ORD(c)]);
        INC(n)
      END
    END;
    INC(a[LAST(a)]);
    FromCards(n, a, res);
    RETURN res
  END FromText;

PROCEDURE FromCards(n : CARDINAL; READONLY a : CardArr; VAR res : T) =
  VAR
    nn := n;
    sa := 0;
  BEGIN
    WHILE nn > LAST(Bits.Freq) DO
      nn := Word.RightShift(nn, 1);
      INC(sa)
    END;

    FOR i := FIRST(a) TO LAST(a) DO
      IF a[i] = 0 THEN
        res[i] := 0
      ELSE
        res[i] := MAX(Word.RightShift(a[i], sa), 1)
      END
    END
  END FromCards;

PROCEDURE Unzero(VAR t : T) =
  BEGIN
    FOR i := FIRST(t) TO LAST(t) DO
      IF t[i] = 0 THEN t[i] := 1 END
    END
  END Unzero;

BEGIN END FreqTable.
