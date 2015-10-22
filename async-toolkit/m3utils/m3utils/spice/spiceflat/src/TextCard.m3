MODULE TextCard;
IMPORT Text, Integer;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN Text.Equal(a.t,b.t) AND a.c=b.c END Equal;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN 
    WITH ires = Integer.Compare(a.c, b.c) DO
      IF ires # 0 THEN RETURN ires ELSE RETURN Text.Compare(a.t,b.t) END
    END
  END Compare;

BEGIN END TextCard.
