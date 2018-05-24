MODULE StructField;
IMPORT Word;
FROM Fmt IMPORT F, Unsigned, Pad;

PROCEDURE Format(v : Word.T; wid : [1..BITSIZE(Word.T)]) : TEXT =
  VAR
    chars := (wid-1) DIV 4 + 1;
  BEGIN
    RETURN F("16_%s",Pad(Unsigned(v,base:=16),
                         length:= chars,
                         padChar:= '0'))
  END Format;

BEGIN END StructField.
