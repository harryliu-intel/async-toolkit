MODULE FData;
IMPORT Field;
FROM Debug IMPORT UnNil;
FROM Fmt   IMPORT F;

PROCEDURE Format(READONLY t : T) : TEXT =
  BEGIN
    RETURN F("{ %s %s %s }",
             Field.Names[t.type],
             UnNil(t.name),
             UnNil(t.formula))
  END Format;
  
BEGIN
END FData.
