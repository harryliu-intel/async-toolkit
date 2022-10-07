MODULE HnnHrep;
IMPORT Word;

PROCEDURE SetHash(VAR t : T) =
  VAR
    h : Word.T := 0;
  BEGIN
    FOR i := FIRST(t.bits^) TO LAST(t.bits^) DO
      h := Word.Plus(h, t.bits[i])
    END;
    t.hashV := h;
    t.hashValid := TRUE
  END SetHash;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    IF a.hashValid AND b.hashValid AND a.hashV # b.hashV THEN
      RETURN FALSE
    ELSIF NUMBER(a.bits^) # NUMBER(b.bits^) OR a.sz # b.sz THEN
      RETURN FALSE
    ELSE
      RETURN a.bits^ = b.bits^
    END
  END Equal;

PROCEDURE Hash(VAR a : T) : Word.T =
  BEGIN
    IF NOT a.hashValid THEN
      SetHash(a)
    END;
    RETURN a.hashV
  END Hash;

BEGIN END HnnHrep.
