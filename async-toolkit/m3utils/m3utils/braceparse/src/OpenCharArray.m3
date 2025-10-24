MODULE OpenCharArray;
IMPORT Word;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    IF NUMBER(a) # NUMBER(b) THEN
      RETURN FALSE
    ELSE
      RETURN a = b
    END
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      res := Word.Plus(res, ORD(a[i]))
    END;
    RETURN res
  END Hash;

PROCEDURE Clone(READONLY a : T) : REF T =
  (* seems to me this could be in the generic module and not here *)
  VAR
    res := NEW(REF T, NUMBER(a));
  BEGIN
    res^ := a;
    RETURN res
  END Clone;
  
BEGIN END OpenCharArray.
