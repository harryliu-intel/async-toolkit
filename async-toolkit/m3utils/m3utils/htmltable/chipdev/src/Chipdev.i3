(* $Id$ *)

INTERFACE Chipdev;
IMPORT DBerr;

PROCEDURE InitDB();
  (* call this right after initializing database *)

PROCEDURE StartURL() : TEXT;

END Chipdev.
