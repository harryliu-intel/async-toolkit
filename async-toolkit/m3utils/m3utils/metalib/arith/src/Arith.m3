MODULE Arith;
IMPORT Word;

VAR mu := NEW(MUTEX);
    nextId : Word.T := 1;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN
    IF a.id = 0 THEN 
      LOCK mu DO a.id := nextId; INC(nextId) END
    END;
    RETURN a.id 
  END Hash;

BEGIN END Arith.
