INTERFACE StdfRd;
IMPORT Rd;

EXCEPTION ShortRead;

PROCEDURE GetChars(rd : Rd.T; VAR len : CARDINAL; VAR x : ARRAY OF CHAR)
  RAISES { ShortRead };
  (* fill the array x with characters.
     If len hits zero or the reader runs out of characters, raises ShortRead.
     
  *)

CONST Brand = "StdfRd";

END StdfRd.
