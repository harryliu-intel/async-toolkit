(* $Id$ *)

INTERFACE Fileviewer;

PROCEDURE InitDB();
  (* call this right after initializing database *)

PROCEDURE StartURL() : TEXT;

END Fileviewer.
