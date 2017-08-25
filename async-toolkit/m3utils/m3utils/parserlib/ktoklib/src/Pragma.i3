(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Pragma.i3,v 1.2 2001/07/29 21:57:45 kp Exp $ *)

INTERFACE Pragma;
IMPORT Rd;
CONST
  Brand = "Pragma";
TYPE
  T = OBJECT
  METHODS
    do(rd: Rd.T);
    (* on entry: reader positionned after "%pragma" or line start ("") *)
    (* on exit: reader positionned after pragma arguments *)
  END;
END Pragma.
