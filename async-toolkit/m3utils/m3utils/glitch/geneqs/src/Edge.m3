MODULE Edge;
IMPORT Text;

CONST TE = Text.Equal;
      
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN TE(a.from, b.from) AND TE(a.to, b.to) END Equal;

BEGIN END Edge.
