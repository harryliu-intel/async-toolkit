INTERFACE BraceParse;
IMPORT Rd, Thread;

PROCEDURE Parse(rd : Rd.T)
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };
  
END BraceParse.
