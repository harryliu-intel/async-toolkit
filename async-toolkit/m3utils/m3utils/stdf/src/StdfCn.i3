INTERFACE StdfCn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Wr;

TYPE T = REF ARRAY OF CHAR;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure };

PROCEDURE Bytes(READONLY t : T) : CARDINAL;

CONST Brand = "StdfCn";

PROCEDURE Default() : T;
  
END StdfCn.
