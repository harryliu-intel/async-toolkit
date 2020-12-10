MODULE CharNames;
IMPORT CharTextTbl;
IMPORT TextSetDef;

VAR
  tab := NEW(CharTextTbl.Default).init();
  check := NEW(TextSetDef.T).init();

PROCEDURE Map(c : CHAR; VAR to : TEXT) : BOOLEAN =
  BEGIN
    RETURN tab.get(c, to)
  END Map;
  
BEGIN
  FOR i := FIRST(Mappings) TO LAST(Mappings) DO
    WITH m     = Mappings[i],
         hadIt1 = tab.put(m.c, m.nm),
         hadIt2 = check.insert(m.nm) DO
      <*ASSERT NOT hadIt1*>
      <*ASSERT NOT hadIt2*>
    END
  END
END CharNames.
