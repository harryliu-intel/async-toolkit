INTERFACE StdfN1;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Wr;

TYPE T = [0..15];

PROCEDURE ParseArray(rd : Rd.T; VAR len : CARDINAL; VAR t : ARRAY OF T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;

PROCEDURE WriteArray(wr : Wr.T; READONLY t : ARRAY OF T)
  RAISES { Thread.Alerted, Wr.Failure };

PROCEDURE BytesArray(READONLY t : ARRAY OF T) : CARDINAL;
  
CONST Brand = "StdfN1";

END StdfN1.
