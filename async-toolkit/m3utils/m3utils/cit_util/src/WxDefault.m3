MODULE WxDefault EXPORTS Wx;
REVEAL
  T = TextWr.T BRANDED "WxDefault" OBJECT;

PROCEDURE PutChar (t: T;  ch: CHAR) =
  BEGIN Wr.PutChar(t, ch); END PutChar;
PROCEDURE PutText (t: T;  txt: TEXT) =
  BEGIN Wr.PutText(t, txt); END PutChar;
PROCEDURE PutInt  (t: T;  i: INTEGER) =
  BEGIN Wr.PutInt(t, Fmt.Int(i)); END PutInt;
PROCEDURE PutStr  (t: T;  READONLY x: ARRAY OF CHAR) =
  BEGIN Wr.PutChars(t, x); END PutStr;

PROCEDURE ToText   (t: T): TEXT =
  BEGIN RETURN TextWr.ToText(t); END ToText;

BEGIN
END WxDefault.
