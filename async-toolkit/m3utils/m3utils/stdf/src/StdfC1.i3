INTERFACE StdfC1;
IMPORT Rd, StdfE, Thread;
IMPORT Wr;

TYPE T = ARRAY [0..1-1] OF CHAR;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "StdfC1";

PROCEDURE Write(wr : Wr.T; READONLY t : T) RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE Bytes(READONLY t : T) : CARDINAL;

END StdfC1.
