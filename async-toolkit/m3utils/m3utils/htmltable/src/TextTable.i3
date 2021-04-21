INTERFACE TextTable;
IMPORT TextTextTbl;
IMPORT HTMLTable;

TYPE 
  T <: Public;

  Public = TextTextTbl.Default OBJECT
  METHODS
    toHTML() : HTMLTable.T;
  END;

END TextTable.
