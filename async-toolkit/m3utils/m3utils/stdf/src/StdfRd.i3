INTERFACE StdfRd;
IMPORT Rd;
IMPORT StdfE;
IMPORT Thread;

PROCEDURE Chars(rd : Rd.T; VAR len : CARDINAL; VAR x : ARRAY OF CHAR)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };
  (* fill the array x with characters.
     If len hits zero or the reader runs out of characters, raises StdfE.E
   *)

PROCEDURE Char(rd : Rd.T; VAR len : CARDINAL) : CHAR
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE U1(rd : Rd.T; VAR len : CARDINAL) : [ 0..255 ]
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE U2(rd : Rd.T; VAR len : CARDINAL) : [ 0..65535 ]
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

CONST Brand = "StdfRd";

END StdfRd.
