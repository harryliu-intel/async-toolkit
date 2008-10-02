(* $Id$ *)

MODULE SchemeString;
IMPORT Text;

PROCEDURE FromText(txt : TEXT) : T =
  VAR     str := NEW(T, Text.Length(txt));
  BEGIN
    FOR i := FIRST(str^) TO LAST(str^) DO
      str[i] := Text.GetChar(txt,i)
    END;
    RETURN str
  END FromText;

BEGIN END SchemeString.
