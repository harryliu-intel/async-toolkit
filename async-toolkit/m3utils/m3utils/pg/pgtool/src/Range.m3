MODULE Range;
FROM Fmt IMPORT Int, F;
IMPORT Integer;
IMPORT Word;
IMPORT Text;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a.lo = b.lo   AND
               a.len = b.len AND
               Text.Equal(a.group, b.group)
  END Equal;

PROCEDURE Overlap(READONLY a, b : T) : BOOLEAN =
  VAR
    alim := a.lo + a.len;
    blim := b.lo + b.len;
  BEGIN
    RETURN a.lo >= b.lo AND a.lo < blim
       OR
           b.lo >= a.lo AND b.lo < alim
  END Overlap;

PROCEDURE CanMerge(READONLY a, b : T; VAR c : T) : BOOLEAN =
  BEGIN
    IF    a.lo + a.len = b.lo AND Text.Equal(a.group, b.group) THEN
      c := NEW(T);
      c^ := B { a.lo, a.len + b.len, a.group };
      RETURN TRUE
    ELSIF b.lo + b.len = a.lo AND Text.Equal(a.group, b.group) THEN
      c := NEW(T);
      c^ := B { b.lo, a.len + b.len, a.group };
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END CanMerge;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("%s + %s (=%s) { %s }:",
             Int(a.lo, base := 16),
             Int(a.len, base := 16),
             Int(a.lo + a.len, base := 16),
             a.group)
  END Format;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN RETURN a.lo END Hash;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN RETURN Integer.Compare(a.lo, b.lo) END Compare;
  
BEGIN END Range.
