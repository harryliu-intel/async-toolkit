INTERFACE StdfVn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Wr;

TYPE T = REF ARRAY OF CHAR;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, StdfE.Missing, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfVn";

PROCEDURE Default() : T;

PROCEDURE Bytes(READONLY t : T) : CARDINAL;
  
PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure };

END StdfVn.
