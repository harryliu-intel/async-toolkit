MODULE RegScalaConstants;
FROM Fmt IMPORT F;

PROCEDURE IdiomName(txt : TEXT; debug : BOOLEAN) : TEXT =
  VAR
    res : TEXT;
  BEGIN
    res := txt;

    (* IdStyles.Convert(txt,
                            IdStyles.Case.Upper,
                            IdStyles.Case.Camel,
                            IdStyles.Sep.Underscore,
                            IdStyles.Sep.None);*)

    IF debug THEN res := res & F("/*%s*/",txt) END;
    RETURN res
  END IdiomName;

BEGIN END RegScalaConstants.
