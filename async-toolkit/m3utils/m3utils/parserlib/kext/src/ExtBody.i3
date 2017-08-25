(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ExtBody.i3,v 1.2 2001/07/29 21:55:11 kp Exp $ *)

INTERFACE ExtBody;
IMPORT LoadSpec;
IMPORT TextSubs;
IMPORT Rd;
TYPE
  T = TextSubs.T;
PROCEDURE Parse(from: Rd.T; READONLY spec: LoadSpec.Info): T;
END ExtBody. 
