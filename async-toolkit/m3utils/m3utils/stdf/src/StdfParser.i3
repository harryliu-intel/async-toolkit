INTERFACE StdfParser;
IMPORT Rd;
IMPORT StdfRecordObjectSeq;
IMPORT Thread;
IMPORT StdfE;

PROCEDURE Parse(rd : Rd.T) : StdfRecordObjectSeq.T
  RAISES { StdfE.E, Rd.Failure, Thread.Alerted };

CONST Brand = "StdfParser";
      
END StdfParser.
