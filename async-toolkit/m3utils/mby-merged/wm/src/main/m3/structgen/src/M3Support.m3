MODULE M3Support;
IMPORT Word;
IMPORT Scan, Text;
IMPORT Fmt;
IMPORT FloatMode, Lex;
IMPORT TextUtils;
IMPORT Debug;

PROCEDURE ReformatNumber(txt : TEXT) : TEXT
  RAISES { Lex.Error, FloatMode.Trap } =
  BEGIN
    RETURN "16_" & Fmt.Unsigned(ParseUnsigned(txt), base := 16)
  END ReformatNumber;
  
PROCEDURE ReformatNumberScala(txt : TEXT) : TEXT
  RAISES { Lex.Error, FloatMode.Trap } =
  BEGIN
    RETURN "0x" & Fmt.Unsigned(ParseUnsigned(txt), base := 16)
  END ReformatNumberScala;
  
PROCEDURE ParseUnsigned(q : TEXT) : Word.T
  RAISES { Lex.Error, FloatMode.Trap } =
  VAR
    str := TextUtils.ToLower(q);
    usp := Text.FindChar(str, '_');
    n := Text.Length(str);
  BEGIN
    IF n > 2 AND Text.Equal(Text.Sub(str, 0, 2), "0x") THEN
      RETURN Scan.Unsigned(Text.Sub(str, 2), defaultBase := 16)
    ELSIF usp = -1 THEN
      RETURN Scan.Unsigned(str, defaultBase := 10)
    ELSE
      RETURN Scan.Unsigned(Text.Sub(str,usp+1),
                           defaultBase := Scan.Int(Text.Sub(str,0,usp),
                                                   defaultBase := 10))
    END
  END ParseUnsigned;


PROCEDURE Modula3Type(ofBits : [1..BITSIZE(Word.T)]) : TEXT =
  BEGIN
    CASE ofBits OF
      1..BITSIZE(Word.T)-1 =>
      RETURN Fmt.F("[0..16_%s]",Fmt.Unsigned(Word.Shift(1,ofBits)-1,base:=16))
    |
      BITSIZE(Word.T) =>
      RETURN "Word.T"
    ELSE
      Debug.Error("Field too wide");
      <*ASSERT FALSE*>
    END
  END Modula3Type;

BEGIN END M3Support.
