MODULE RAC;
IMPORT Word;
IMPORT Text;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN RETURN NUMBER(a^) = NUMBER(b^) AND a^ = b^ END Equal;

PROCEDURE EqualB(READONLY a, b : B) : BOOLEAN =
  BEGIN RETURN NUMBER(a) = NUMBER(b) AND a = b END EqualB;

PROCEDURE Hash(a : T) : Word.T =
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := FIRST(a^) TO LAST(a^) DO
      res := Word.Plus(res, ORD(a[i]))
    END;
    RETURN res
  END Hash;

PROCEDURE Compare(a, b : T) : [-1 .. 1] =
  VAR
    n := MIN(NUMBER(a^), NUMBER(b^));
  BEGIN
    FOR i := 0 TO n - 1 DO
      IF    a[i] < b[i] THEN
        RETURN -1
      ELSIF a[i] > b[i] THEN
        RETURN +1
      END
    END;
    IF    NUMBER(a^) > NUMBER(b^) THEN
      RETURN +1
    ELSIF NUMBER(a^) < NUMBER(b^) THEN
      RETURN -1
    ELSE
      RETURN 0
    END
  END Compare;

PROCEDURE ToText(a : T) : TEXT =
  BEGIN
    RETURN Text.FromChars(a^)
  END ToText;

PROCEDURE FromText(txt : TEXT) : T =
  BEGIN
    WITH res = NEW(T, Text.Length(txt)) DO
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := Text.GetChar(txt, i)
      END;
      RETURN res
    END
  END FromText;

PROCEDURE Sub(t : T; from : CARDINAL; len : CARDINAL := LAST(CARDINAL)) : T =
  VAR
    n := NUMBER(t^);
    d := MAX(0, MIN(n - from, len));
    res := NEW(T, d);
  BEGIN
    IF d # 0 THEN
      res^ := SUBARRAY(t^, from, d)
    END;
    RETURN res
  END Sub;

PROCEDURE SubIs(t, u : T;
                from : CARDINAL;
                len : CARDINAL := LAST(CARDINAL)) : BOOLEAN =
  VAR
    n := NUMBER(t^);
    d := MAX(0, MIN(n - from, len));
  BEGIN
    RETURN EqualB(SUBARRAY(t^, from, d), u^)
  END SubIs;

PROCEDURE FromB(READONLY b : B) : T =
  VAR
    res := NEW(T, NUMBER(b));
  BEGIN
    res^ := b;
    RETURN res
  END FromB;

BEGIN END RAC.
