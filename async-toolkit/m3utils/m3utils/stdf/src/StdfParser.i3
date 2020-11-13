INTERFACE StdfParser;
IMPORT Rd;
IMPORT StdfRecordObjectSeq;

PROCEDURE Parse(rd : Rd.T) : StdfRecordObjectSeq;

CONST Brand = "StdfParser";
      
END StdfParser.
