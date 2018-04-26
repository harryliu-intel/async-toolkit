MODULE NetTypes;
IMPORT Word;
IMPORT Fmt;
IMPORT Text;

PROCEDURE Format(u : Word.T; wid : [1..BITSIZE(Word.T) DIV 4] ) : TEXT  =
  BEGIN
    WITH digits = Fmt.Int(u, base := 16) DO
      <*ASSERT Text.Length(digits) <= wid*>
      <*ASSERT wid <= BITSIZE(Word.T) DIV 4*>
      RETURN "0x" & Fmt.Pad(digits, length := wid, padChar := '0')
    END
  END Format;

PROCEDURE FormatU8(u : U8) : TEXT =
  BEGIN RETURN Format(u, 2) END FormatU8; 

PROCEDURE FormatU16(u : U16) : TEXT =
  BEGIN RETURN Format(u, 4) END FormatU16;

PROCEDURE FormatU32(u : U32) : TEXT =
  BEGIN RETURN Format(u, 8) END FormatU32;

BEGIN END NetTypes.
