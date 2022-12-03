MODULE FreqTable;
IMPORT ArithProbability;

PROCEDURE GetProbability(READONLY cum : Cum; c : CHAR) : ArithProbability.T =
  BEGIN
    RETURN ArithProbability.T { cum[ORD(c)], cum[ORD(c) + 1], cum[LAST(cum)] }
  END GetProbability;

PROCEDURE Accumulate(READONLY t : T; VAR cum : Cum) =
  BEGIN
    cum := Cum { 0, .. };

    cum[0] := t[0];

    FOR i := FIRST(t) + 1 TO LAST(t) DO
      cum[i] := cum[i - 1] + t[i];
      cum[LAST(cum)] := cum[LAST(cum)] + t[i]
    END
  END Accumulate;

BEGIN END FreqTable.
