INTERFACE TextCard;

TYPE
  T = RECORD
    t : TEXT;
    c : CARDINAL;
  END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
PROCEDURE Compare(READONLY a, b : T) : [-1..1]; (* compare by c, then t *)

CONST Brand = "TextCard";

END TextCard.
