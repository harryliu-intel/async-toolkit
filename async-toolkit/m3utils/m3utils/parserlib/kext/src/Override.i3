INTERFACE Override;
IMPORT TextTextTbl;
IMPORT Rd;
TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(specs: TextTextTbl.T): T;
    add(name, body: TEXT; rd: Rd.T);
    importRemaining();
    getProcAssignText(): TEXT;
    getText(): TEXT;
    overridden(name: TEXT): BOOLEAN;
  END;
END Override.
    
