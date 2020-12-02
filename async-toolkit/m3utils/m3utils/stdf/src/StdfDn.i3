INTERFACE StdfDn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Wr;

TYPE T = REF ARRAY OF BOOLEAN;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.Missing, StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure };

PROCEDURE Bytes(READONLY t : T) : CARDINAL;

CONST Brand = "StdfDn";

PROCEDURE Default() : T;
  
END StdfDn.
