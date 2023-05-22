MODULE TechLookup;
IMPORT Text;
IMPORT Debug; FROM Debug IMPORT UnNil;
FROM Fmt IMPORT F;

CONST TE = Text.Equal;

PROCEDURE Lookup(str : TEXT; READONLY a : ARRAY OF TEXT) : CARDINAL =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF TE(str, a[i]) THEN RETURN i END
    END;
    VAR
      msg := F("could not find %s among alternatives : ",
               UnNil(str));
    BEGIN
      FOR i := FIRST(a) TO LAST(a) DO
        msg := msg & F( " \"%s\"" , UnNil(a[i]))
      END;
      Debug.Error(msg)
    END;
    <*ASSERT FALSE*>
  END Lookup;

BEGIN END TechLookup.
