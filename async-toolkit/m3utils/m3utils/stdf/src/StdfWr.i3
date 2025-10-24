INTERFACE StdfWr;
IMPORT Wr;
IMPORT Thread;

(*
PROCEDURE Chars(wr : Wr.T; READONLY x : ARRAY OF CHAR)
  RAISES { Thread.Alerted, Wr.Failure };
*)

CONST Chars = Wr.PutString;

(*
PROCEDURE Char(wr : Wr.T; READONLY x : CHAR)
  RAISES { Thread.Alerted, Wr.Failure };
  *)

CONST Char = Wr.PutChar;

PROCEDURE U1(wr : Wr.T; READONLY x : [0..255])
  RAISES { Thread.Alerted, Wr.Failure };

PROCEDURE U2(wr : Wr.T; READONLY x : [0..65535])
  RAISES { Thread.Alerted, Wr.Failure };

CONST Brand = "StdfWr";

END StdfWr.

