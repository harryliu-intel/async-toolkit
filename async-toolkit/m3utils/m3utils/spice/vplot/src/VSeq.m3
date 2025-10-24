MODULE VSeq;
IMPORT Text, Word;
IMPORT Integer;

CONST TE = Text.Equal;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    IF a.size() # b.size() THEN RETURN FALSE END;
    FOR i := 0 TO a.size()-1 DO
      IF NOT TE(a.get(i),b.get(i)) THEN RETURN FALSE END
    END;
    RETURN TRUE
  END Equal;

PROCEDURE Compare(a, b : T) : [-1..1] =
  VAR 
    tent : [-1..1] := 0;
  BEGIN
    tent := Integer.Compare(a.size(),b.size());
    IF tent # 0 THEN RETURN tent END;
    FOR i := 0 TO a.size()-1 DO
      tent := Text.Compare(a.get(i),b.get(i));
      IF tent # 0 THEN RETURN tent END
    END;
    RETURN tent
  END Compare;

PROCEDURE Hash(a : T) : Word.T =
  VAR
    w : Word.T := 0;
  BEGIN
    FOR i := 0 TO a.size()-1 DO
      w := Word.Plus(w, Text.Hash(a.get(i)))
    END;
    RETURN w
  END Hash;

BEGIN END VSeq.
