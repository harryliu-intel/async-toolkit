MODULE CheckDir;
IMPORT Transition;
FROM Fmt IMPORT Int;

PROCEDURE Fmt(c : T) : TEXT =
  VAR
    res := "{ ";
  BEGIN
    FOR i := FIRST(Transition.Dir) TO LAST(Transition.Dir) DO
      IF i IN c THEN
        res := res & Int(i) & " "
      END
    END;

    RETURN res & "}"
  END Fmt;

BEGIN END CheckDir.
