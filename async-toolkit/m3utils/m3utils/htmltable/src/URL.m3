MODULE URL;
IMPORT Text, FloatMode, Lex;
IMPORT Scan;

PROCEDURE PlusToSpace(this : TEXT) : TEXT =
  VAR
    chars := NEW(REF ARRAY OF CHAR, Text.Length(this));
  BEGIN
    FOR i := 0 TO LAST(chars^) DO
      WITH c = Text.GetChar(this,i) DO
        IF c = '+' THEN chars[i] := ' '
        ELSE chars[i] := c
        END
      END
    END;
    RETURN Text.FromChars(chars^)
  END PlusToSpace;

PROCEDURE Unescape(this : TEXT) : TEXT 
  RAISES { FloatMode.Trap, Lex.Error } =
  VAR
    len := Text.Length(this);
    chars := NEW(REF ARRAY OF CHAR, len);
    thisIndex, charsIndex : CARDINAL := 0;
    arrayLen := len;
  BEGIN
    WHILE thisIndex < len DO
      WITH c = Text.GetChar(this, thisIndex) DO
        IF c = '%' THEN
          thisIndex := thisIndex + 1;
          VAR
            hex := Scan.Int(Text.Sub(this, thisIndex, 2),16);
          BEGIN
            chars[charsIndex] := VAL(hex,CHAR);
            thisIndex := thisIndex + 2;
            charsIndex := charsIndex + 1;
            arrayLen := arrayLen - 2; (* we lost this many *)
          END
        ELSE
          chars[charsIndex] := c;
          thisIndex := thisIndex + 1;
          charsIndex := charsIndex + 1
        END
      END
    END;
    RETURN Text.FromChars(SUBARRAY(chars^,0,arrayLen))
  END Unescape;

BEGIN END URL.
