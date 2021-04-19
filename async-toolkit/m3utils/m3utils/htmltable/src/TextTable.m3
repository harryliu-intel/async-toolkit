MODULE TextTable;
IMPORT HTMLTable, Debug;

REVEAL 
  T = Public BRANDED "TextTable" OBJECT
  OVERRIDES
    toHTML := ToHTML;
  END;

PROCEDURE ToHTML(self : T) : HTMLTable.T =
  VAR 
    iterator := self.iterate();
    table := NEW(HTMLTable.T);
    key, value : TEXT;
  BEGIN
    WHILE iterator.next(key,value) DO
      VAR
        row := NEW(REF ARRAY OF TEXT, 2);
      BEGIN
        row^:= ARRAY OF TEXT { key, value };
        Debug.Out("Adding (" & key & "," & value &") to table.");
        table.addRow(row)
      END
      END;
    RETURN table
  END ToHTML;

BEGIN END TextTable.
