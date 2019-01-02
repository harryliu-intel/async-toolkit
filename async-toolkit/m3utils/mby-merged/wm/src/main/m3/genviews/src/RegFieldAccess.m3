MODULE RegFieldAccess;
IMPORT Text;
IMPORT ParseError;

CONST TE = Text.Equal;
      
PROCEDURE Parse(nm : TEXT) : OpSet RAISES { ParseError.E } =
  BEGIN
    FOR i := FIRST(Named) TO LAST(Named) DO
      IF TE(nm, Named[i].nm) THEN RETURN Named[i].opSet END
    END;
    RAISE ParseError.E(nm)
  END Parse;

BEGIN END RegFieldAccess.
