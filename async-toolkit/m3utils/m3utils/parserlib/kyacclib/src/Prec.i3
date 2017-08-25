(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Prec.i3,v 1.2 2001/07/29 21:58:02 kp Exp $ *)

INTERFACE Prec;
CONST
  Brand = "Prec";
TYPE
  Kind = {Left, Right, None};
  T = REF RECORD
    kind: Kind;
    val: INTEGER := 0;
    used: BOOLEAN := FALSE;
  END;

PROCEDURE Format(a: T): TEXT;
END Prec.
