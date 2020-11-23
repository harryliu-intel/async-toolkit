INTERFACE BraceParse;
IMPORT Rd, Thread;

PROCEDURE Parse(rd : Rd.T)
  RAISES { Rd.Failure, Thread.Alerted };
  
END BraceParse.
