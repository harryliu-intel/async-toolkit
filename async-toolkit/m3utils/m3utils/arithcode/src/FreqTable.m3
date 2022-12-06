MODULE FreqTable;
IMPORT ArithProbability;

PROCEDURE GetProbability(READONLY cum : Cum; c : CHAR) : ArithProbability.T =
  BEGIN
    RETURN ArithProbability.T { cum[ORD(c)], cum[ORD(c) + 1], cum[LAST(cum)] }
  END GetProbability;

PROCEDURE Accumulate(READONLY t : T; VAR cum : Cum) =
  BEGIN
    cum := Cum { 0, .. };

    FOR i := FIRST(cum) + 1 TO LAST(cum) DO
      cum[i] := cum[i - 1] + t[i - 1];
    END

  END Accumulate;

BEGIN END FreqTable.
