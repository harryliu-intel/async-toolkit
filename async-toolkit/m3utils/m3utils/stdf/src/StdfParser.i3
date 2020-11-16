INTERFACE StdfParser;
IMPORT Rd;
IMPORT StdfRecordObjectSeq;

PROCEDURE Parse(rd : Rd.T) : StdfRecordObjectSeq.T;

CONST Brand = "StdfParser";
      
END StdfParser.
