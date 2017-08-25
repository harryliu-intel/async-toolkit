(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: FmtTable.i3,v 1.2 2001/07/29 21:57:44 kp Exp $ *)

INTERFACE FmtTable;
(* Format a table as the body of a Modula-3 array constant *)
TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(lmargin: TEXT := "    "): T;
    putText(t: TEXT);   (* put text entry *)
    putInt(i: INTEGER); (* put integer entry *)
    toText(): TEXT;
  END;
END FmtTable.
