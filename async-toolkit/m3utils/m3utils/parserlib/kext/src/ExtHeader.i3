INTERFACE ExtHeader;
IMPORT TextList;
IMPORT Rd;
TYPE
  T = RECORD
    sources, imports: TextList.T;
  END;
PROCEDURE Parse(from: Rd.T): T;
END ExtHeader.


  
